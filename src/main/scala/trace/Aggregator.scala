package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TraceAggregator(hartid: Int)(implicit p: Parameters) extends LazyModule {
  val DEBUG: Boolean = true

  val clientParams =
    TLClientParameters(
      name = s"trace_aggregator_${hartid}",
      sourceId = IdRange(0, 1))
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(clientParams))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val core = new TraceIO().asInput
    })

    val filter = Module(new FilterJumps(after = 1))
    filter.io.in := io.core

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
    val addr = Wire((UInt(0x100004100L, width=64)))
    val data = Wire(init = 0.U(64.W))
    val size = log2Ceil(data.getWidth / 8).U

    // "Ring buffer"
    // TODO: Set address from S/W
    val trace_base = UInt(0x100700000L, width=64)
    val trace_offset = RegInit(UInt(0, width=32))
    val trace_cnt = RegInit(UInt(0))
    val trace_entries = 1024
    when (out.a.fire()) {
      trace_cnt := (trace_cnt + 1.U) & (trace_entries - 1).U
    }
    trace_offset := (trace_cnt << size)

    data := queue.io.deq.bits.insn.iaddr
    addr := trace_base + trace_offset

    val (pflegal, pfbits) = edge.Put(src, addr, size, data)

    val a_gen = Wire(init = Bool(false))
    a_gen := queue.io.deq.valid
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
      when (core.valid) {
        printf("TraceAggregator: C%d: %d [%d] pc=[%x] inst=[%x] DASM(%x)\n",
          core.hartid, core.time(31,0), core.insn.valid && !core.insn.exception,
          core.insn.iaddr, core.insn.insn, core.insn.insn)
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
