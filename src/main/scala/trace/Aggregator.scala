package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreModule, RocketTile}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TraceAggregator(tile: RocketTile, hartid: Int)(implicit p: Parameters) extends LazyModule {
  val DEBUG: Boolean = true

  val clientParams =
    TLClientParameters(
      name = s"trace_aggregator_${hartid}",
      sourceId = IdRange(0, 1))
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(clientParams))))
  val ctrl_module =
    LazyModule(new TLTraceCtrl(TraceCtrlParams(0x50000000 + hartid * 4096)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val core = new MonitorIO().asInput
    })
    val ctrl: TraceCtrlBundle = ctrl_module.module.io
    val tracebuf_full =  RegInit(Bool(false))

    // TODO: flush queues when ctrl.enable is 0
    val filter = Module(new FilterPrivSwitch(mask=1)(before=2, after=0))
    filter.io.in := io.core.trace
    filter.io.in.valid := ctrl.enable && io.core.trace.valid && !tracebuf_full

    val depth: Int = 32
    val queue = Module(new Queue(new TraceIO, depth))

    // We silently drop entries if queue overflows

    val core_valid = RegInit(Bool(false))
    val core = RegEnable(filter.io.out, filter.io.out.valid)
    core_valid := filter.io.out.valid || (core_valid && !queue.io.enq.ready)

    queue.io.enq.valid := core_valid
    queue.io.enq.bits := core

    val (out, edge) = node.out(0)

    val src = UInt(0)
    // ??? TODO: Make reg to avoid timing slack
    val addr = Wire((UInt(0x100004100L, width=64)))
    //val addr_ready = Reg(Bool())
    val data = Wire(init = 0.U(32.W))
    val size: Int = log2Ceil(data.getWidth / 8)

    // "Ring buffer 0"
    val trace_offset = RegInit(UInt(0, width=32))
    val trace_size_mask = ctrl.buf0_mask

    when (!ctrl.enable) {
      trace_offset := 0.U
      tracebuf_full := false.B
    } .elsewhen (out.a.fire()) {
      require((1 << size) == 4)
      val incr = (1 << size).U
      val new_traceoffset = (trace_offset + incr) & trace_size_mask
      trace_offset := new_traceoffset
      tracebuf_full := tracebuf_full || new_traceoffset === 0.U
      printf("TraceAggregator: trace_offset=%x\n", trace_offset)
    }

    ctrl.buf0_full := tracebuf_full

    val atrace = Wire(new DefaultTraceFormat())
    //tile.module.core.io.trace_source.regfile.cfg.regno_smode := Wire(17.U(5.W))
    atrace.register  := core.register
    atrace.timestamp := core.time >> ctrl.clock_shift
    atrace.priv      := core.insn.priv
    data := atrace.asUInt
    //data := queue.io.deq.bits.insn.iaddr
    /* TODO: Require that buf0_addr must be aligned to (trace_size_mask+1) so we can do | instead of + */
    addr := ctrl.buf0_addr + trace_offset

    val (pflegal, pfbits) = edge.Put(src, addr, size.U, data)

    val a_gen = Wire(init = Bool(false))
    a_gen := ctrl.enable && !tracebuf_full && queue.io.deq.valid
    queue.io.deq.ready := out.a.fire() && queue.io.deq.valid

    out.a.bits := pfbits

    // Wire up flow control
    val (a_first, _, _) = edge.firstlast(out.a)
    val (_, _, d_done) = edge.firstlast(out.d)
    val in_flight = RegInit(Bool(false))
    in_flight := out.a.fire() || in_flight && !d_done
    out.a.valid := a_gen && pflegal && (!a_first || !in_flight)
    out.d.ready := Bool(true)

    // Tie off
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
    out.b.ready := Bool(true)

    if (DEBUG) {
      val core = filter.io.out
      when (ctrl.enable && core.valid) {
         printf("TraceAggregator: C%d: %d [%d]=[%x] pc=[%x] priv=[%x] inst=[%x]  reg=[%x] time=[%d] priv=[%x] DASM(%x)\n",
           core.hartid, core.time(31,0), !core.insn.exception, core.insn.cause,
           core.insn.iaddr, core.insn.priv, core.insn.insn,
           core.register, atrace.timestamp, atrace.priv,
           core.insn.insn)
      }
    }
  }


  // l1 is per core, l2 is shared?, dram is shared ...
  // l1 is split in instruction and data cache? do we need 2 trace units?
  // val harts = new Seq

  // programmable in the sense that we can select in which mode the aggregator
  // should work. we really need to think about how to make it programmable or modular
  // one way to do it w/ an fpga would be to define this module as a blackbox and implement it
  // in verilog and use partial reconfiguration ...
  // for now there's only one mode, the 'call-graph mode'
  //val mode
  //
  // where do we put this in rocketchip (for now in each tile) ?!?
}
