package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._

class CoreTraceSourceIO(implicit p: Parameters) extends TraceSourceIO {
  {
    kind := TraceKind.core
  }
}

class CoreTraceSource(insn: TracedInstruction)(implicit p: Parameters) extends TraceSource {
  val io = new CoreTraceSourceIO()(p) {
  }
  //io.valid := insn.valid
}

//class RocketTo
