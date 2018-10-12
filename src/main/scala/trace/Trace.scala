package freechips.rocketchip.trace

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._


/* no ready signal, sink needs to keep up --> pipelining / fifo / or dropping ? */

/* top level need be very generic. which trace data will all have? dsid ? cycle? */

object TraceKind {
  val core :: l1 :: l2 :: dram :: Nil = Enum(UInt(), 4)
}

class TraceIO(implicit p: Parameters) extends CoreBundle()(p)
    /* with HasCoreParameters ??? */ {

  val valid = Bool(OUTPUT)

  //// val kind = UInt(OUTPUT, 8 /* log2ceil last traceiokind enum */ ) /* <-- this is static and should be bound at compile time */
  val kind = UInt(OUTPUT, 8)

  //val k_cpu :: k_l1 :: k_l2 :: k_dram :: Nil = Enum(UInt(), 4)
  //val s_ready :: s_req :: s_wait1 :: s_wait2 :: Nil = Enum(UInt(), 4)

  val time = UInt(OUTPUT, xLen)
  val addr = UInt(OUTPUT, xLen)
  val instr = UInt(OUTPUT, xLen)
  val pc = UInt(OUTPUT, xLen - 1) /* insn is at least 2 bytes?! so we can save one wire ?!? */
  ///* aka tag */
  val dsid = UInt(OUTPUT, 16 /* p(dsidLen) */)

    /* Can we specify everything here and if it's not used by the specific trace source/sink will be optimized away? */
}

class TraceSourceIO(implicit p: Parameters) extends TraceIO()(p) {
}

class TraceSinkIO(implicit p: Parameters) extends TraceIO()(p) {
  //{
  //  this.flip()
  //}
}

abstract class TraceSource(implicit p: Parameters)  extends CoreModule {
///  def io = _io
}

abstract class TraceSink(implicit p: Parameters)  extends CoreModule {
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
