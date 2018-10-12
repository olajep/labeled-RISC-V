package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._

class Aggregator(val core: CoreTraceSource /*, l1: CacheTraceSource ... */) extends Module {
  def io = new Bundle {
//    val valid = Bool(OUTPUT)
//    when (core.io.valid) {
//      printf ("valid\n")
//      valid := core.io.valid
//      /* feed into first stage of aggregator pipeline
//       * ...
//       * send to dram
//       */
//    }
  }
  //}

  // programmable in the sense that we can select in which mode the aggregator
  // should work. we really need to think about how to make it programmable or modular
  // one way to do it w/ an fpga would be to define this module as a blackbox and implement it
  // in verilog and use partial reconfiguration ...
  // for now there's only one mode, the 'call-graph mode'
  //val mode
  //
  // where do we put this in rocketchip ?!?
}
