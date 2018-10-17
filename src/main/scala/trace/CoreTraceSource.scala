package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class CoreTraceSourceIO(implicit p: Parameters) extends TraceSourceIO()(p) {
  // ???: insn etc. here instead of base class
}

object CoreTraceSource {
  def connect(out: CoreTraceSourceIO, csr: CSRFileIO) = {
    val insn = csr.trace(0)
    out.kind  := TraceKind.core
    out.valid := insn.valid || insn.exception
    out.insn  <> insn
    out.time  := csr.time
    out.hartid := csr.hartid
  }
}
