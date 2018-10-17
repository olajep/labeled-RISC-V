package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule

class TraceAggregator(implicit p: Parameters) extends CoreModule()(p) {
  val DEBUG = true

  val io = new TraceIO()(p).flip()

  if (DEBUG) {
    when (io.valid) {
      printf("TraceAggregator: C%d: %d [%d] pc=[%x] inst=[%x]\n",
        io.hartid, io.time(31,0), io.insn.valid && !io.insn.exception,
        io.insn.iaddr, io.insn.insn)
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
