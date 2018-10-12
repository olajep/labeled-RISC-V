package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule

//abstract class TraceAggregatorModule(implicit p: Parameters) extends CoreModule()(p)

//class Aggregator(core: CoreTraceSourceIO /*, l1: CacheTraceSource ... */) extends Module {
class TraceAggregator(implicit p: Parameters) extends CoreModule()(p) {
  val io = new TraceIO()(p).flip()

//  val io = new Bundle {
//    val valid = Bool(OUTPUT)
//    //when (core.valid) {
//    //  printf ("valid\n")
//    //  valid := core.valid
//    //  /* feed into first stage of aggregator pipeline
//    //   * ...
//    //   * send to dram
//    //   */
//    //}
//  }
  //}
  //io.valid := core.valid

  // programmable in the sense that we can select in which mode the aggregator
  // should work. we really need to think about how to make it programmable or modular
  // one way to do it w/ an fpga would be to define this module as a blackbox and implement it
  // in verilog and use partial reconfiguration ...
  // for now there's only one mode, the 'call-graph mode'
  //val mode
  //
  // where do we put this in rocketchip ?!?
}
