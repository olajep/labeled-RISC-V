package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._

class CoreTraceSourceIO(implicit p: Parameters) extends TraceSourceIO {
  {
    //kind := TraceKind.core
  }
}

class CoreTraceSource(io: CoreTraceSourceIO, insn: TracedInstruction) extends TraceSource(io) {
  {
    //io.kind := TraceKind.core
    //io <> insn
  }
}

//class RocketTo
