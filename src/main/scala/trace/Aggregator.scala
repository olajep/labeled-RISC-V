package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TraceAggregator(implicit p: Parameters) extends LazyModule {
  val DEBUG = true

  val params = TLClientParameters(
    name = s"TraceAggregator", // s"TraceAggregator${hartID}"
    sourceId = IdRange(0, 32)) // 32 = fifo depth?
  val node = new TLClientNode(Seq(TLClientPortParameters(Seq(params))))


  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      //val out = outs
      val core = new TraceIO()(p).asInput
    })

    //val (outs, edges) = node.out.unzip

    if (DEBUG) {
      val core = io.core
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
