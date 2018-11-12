package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.{min, max}

abstract class Filter(before: Int = 0, after: Int = 0)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new TraceIO)
    val out = new TraceIO
  })


  // Return false if should be filtered away
  def filter(trace: TraceIO): Bool

  // Pipeline
  val buf = Reg(new TraceIO)
  buf := io.in

  val pipe = ShiftRegister(buf, before + 1, buf.valid)

  val matches = Wire(Bool())
  matches := buf.valid && filter(buf)

  val counter = RegInit(UInt(0))
  val depth = after + 1 + before

  when (matches) {
    counter := depth.U
  } .elsewhen (counter =/= 0.U && buf.valid) {
    counter := counter - 1.U
  }

  io.out := pipe
  io.out.valid := buf.valid && pipe.valid && counter =/= 0.U
}

class FilterJumps(before: Int = 0, after: Int = 0)(implicit p: Parameters) extends Filter(before, after)(p) {
  import freechips.rocketchip.rocket.Instructions._
  def filter(trace: TraceIO): Bool = {
    val insn = trace.insn.insn
    val pattern = Seq(JAL, JALR, C_JR , C_JALR)

    pattern.foldLeft(Bool(false)) (_ || _ === insn)
  }
}
