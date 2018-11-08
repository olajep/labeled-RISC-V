package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

abstract class Filter()(implicit p: Parameters) extends Module {
 val io = IO(new Bundle {
  val in = Flipped(new TraceIO)
  val out = new TraceIO
 })

  // Return false if should be filtered away
  def filter(trace: TraceIO): Bool

  // Pipeline
  val buf = Reg(new TraceIO)

  buf := io.in
  io.out := buf
  io.out.valid := buf.valid && filter(buf)
}

class FilterJumps()(implicit p: Parameters) extends Filter()(p) {
  import freechips.rocketchip.rocket.Instructions._
  def filter(trace: TraceIO): Bool = {
    val insn = trace.insn.insn
    val pattern = Seq(JAL, JALR, C_JR , C_JALR)

    pattern.foldLeft(Bool(false)) (_ || _ === insn)
  }
}
