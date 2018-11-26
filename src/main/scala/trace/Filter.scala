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

  // Return false if should be filtered away. Input is not guarded by valid so
  // it must be checked if the function is keeping any state.
  def filter(trace: TraceIO): Bool

  // Pipeline
  val buf = Reg(new TraceIO)
  buf := io.in

  val queue_depth = before + 1
  val queue = Module(new Queue(new TraceIO, queue_depth, flow=true))

  val matches = Wire(Bool())
  matches := buf.valid && filter(buf)

  val counter = RegInit(UInt(0))
  val depth = after + 1 + before

  queue.io.enq.bits := buf
  queue.io.enq.valid := buf.valid

  when (matches) {
    when (counter === 0.U) {
      counter := depth.U - (queue_depth.U - queue.io.count)
    } .elsewhen (counter <= after.U) {
      counter := after.U
    } .otherwise {
      counter := counter
    }
  } .elsewhen (counter =/= 0.U && queue.io.deq.valid) {
    counter := counter - 1.U
  }

  val dequeue = Wire(Bool())
  val is_full = Wire(Bool())
  dequeue := counter =/= 0.U || matches
  is_full := queue.io.count >= (queue_depth - 1).U

  io.out := queue.io.deq.bits
  io.out.valid := queue.io.deq.valid && dequeue

  queue.io.deq.ready := is_full || dequeue
}

class FilterJumps(before: Int = 0, after: Int = 0)(implicit p: Parameters) extends Filter(before, after)(p) {
  import freechips.rocketchip.rocket.Instructions._
  def filter(trace: TraceIO): Bool = {
    val insn = trace.insn.insn
    val pattern = Seq(JAL, JALR, C_JR , C_JALR)

    pattern.foldLeft(Bool(false)) (_ || _ === insn)
  }
}

class FilterPrivSwitch(mask: Int = PRV.M) (before: Int = 1, after: Int = 0) (implicit p: Parameters) extends Filter(before, after)(p) {
  def filter(trace: TraceIO): Bool = {
    val priv = trace.insn.priv & mask.U
    val prev_priv = RegEnable(priv, trace.valid) /* { reg_debug, reg_mstatus.prv } */

    prev_priv =/= priv
  }
}
