package freechips.rocketchip.trace

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._


/* no ready signal, sink needs to keep up --> pipelining / fifo / or dropping ? */

/* top level need be very generic. which trace data will all have? dsid ? cycle? */

object TraceKind {
  val core :: l1 :: l2 :: dram :: Nil = Enum(UInt(), 4)
}

class TraceIO(implicit p: Parameters) extends CoreBundle()(p) {

  // What type of trace (should be compile time static?!)
  val kind = UInt(OUTPUT, 8)

  // True when a valid trace is on the wire, false otherwise.
  val valid = Bool(OUTPUT)

  val hartid = UInt(OUTPUT, hartIdLen)
  val insn = new TracedInstruction()(p)

  val time = UInt(OUTPUT, xLen)

  //// val kind = UInt(OUTPUT, 8 /* log2ceil last traceiokind enum */ ) /* <-- this is static and should be bound at compile time */

  //val k_cpu :: k_l1 :: k_l2 :: k_dram :: Nil = Enum(UInt(), 4)
  //val s_ready :: s_req :: s_wait1 :: s_wait2 :: Nil = Enum(UInt(), 4)

  //val dsid = UInt(OUTPUT, 16 /* p(dsidLen) */)

  /* require (enableCommitLog > 32, "we don't support timer overflow yet ... */

  //val addr = UInt(OUTPUT, xLen)
  //val instr = UInt(OUTPUT, xLen)
  //val pc = UInt(OUTPUT, xLen - 1) /* insn is at least 2 bytes?! so we can save one wire ?!? */
  /////* aka tag */

    /* Can we specify everything here and if it's not used by the specific trace source/sink will be optimized away? */
}

abstract class TraceSourceIO(implicit p: Parameters) extends TraceIO()(p) {
}

abstract class TraceSinkIO(implicit p: Parameters) extends TraceIO()(p) {
  /* { this.flip() } <-- doesn't work */
}

abstract class TraceSource(implicit p: Parameters) extends CoreModule()(p) {
///  def io = _io
}

abstract class TraceSink(implicit p: Parameters) extends CoreModule()(p) {
//  def io = _io
}

//abstract class TraceSink(io: CoreTraceIO) extends Module {
//  val io = new TraceIO().flip
//}

/* Make the below thing programmable? */
/* class TraceAggregator extends CoreModule ... <-- own file? */

// trait HasTraceSource {
//   /* this: Module => */ /* ??? */
//   val valid = Bool(OUTPUT)
// }
//
// trait HasTraceSink {
//   /* this: Module => */ /* ??? */
//   val valid = Bool(INPUT)
// }

//class WithTracing extends Config((site, here, up) => {
//  /* ??? */
//})
