package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreModule, RocketTile}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TraceAggregatorBundle(implicit p: Parameters) extends Bundle {
  val coremon = new MonitorIO().asInput
}

class TraceAggregatorModule(outer: TraceAggregator) extends LazyModuleImp(outer) {
  val DEBUG: Boolean = true
  val io = IO(new TraceAggregatorBundle)
  val ctrl: TraceCtrlBundle = outer.ctrl_module.module.io
  val tracebuf_full =  RegInit(Bool(false))

  // TODO: flush queues when ctrl.enable is 0
  val filter = Module(new FilterPrivSwitch(mask=1)(before=2, after=0))
  filter.io.in := io.coremon.trace
  filter.io.in.valid := ctrl.enable && io.coremon.trace.valid && !tracebuf_full

  val depth: Int = 32
  val queue = Module(new Queue(new TraceIO, depth))

  // We silently drop entries if queue overflows

  val coretrace_valid = RegInit(Bool(false))
  val coretrace = RegEnable(filter.io.out, filter.io.out.valid)
  coretrace_valid :=
    filter.io.out.valid || (coretrace_valid && !queue.io.enq.ready)

  queue.io.enq.valid := coretrace_valid
  queue.io.enq.bits := coretrace

  val (out, edge) = outer.node.out(0)

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
  atrace.register  := queue.io.deq.bits.register
  atrace.timestamp := queue.io.deq.bits.time >> ctrl.clock_shift
  atrace.priv      := queue.io.deq.bits.insn.priv
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
    when (out.a.fire()) {
      val t = queue.io.deq.bits
      printf("TraceAggregator: C%d: %d [%d]=[%x] pc=[%x] priv=[%x] inst=[%x] " +
             "reg=[%x] time=[%d] priv=[%x] DASM(%x)\n",
             t.hartid, t.time(31,0), !t.insn.exception, t.insn.cause,
             t.insn.iaddr, t.insn.priv, t.insn.insn,
             atrace.register, atrace.timestamp, atrace.priv,
             t.insn.insn)
    }
  }
}

class TraceAggregator(tile: RocketTile, hartid: Int)(implicit p: Parameters)
                      extends LazyModule {
  val clientParams =
    TLClientParameters(
      name = s"trace_aggregator_${hartid}",
      sourceId = IdRange(0, 1))
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(clientParams))))
  val ctrl_module =
    LazyModule(new TLTraceCtrl(TraceCtrlParams(0x50000000 + hartid * 4096)))

  lazy val module = new TraceAggregatorModule(this)
}
