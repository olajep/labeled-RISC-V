package freechips.rocketchip.trace

import Chisel._
import chisel3.core.{Input, Output}

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

class TraceCfgIO extends Bundle {
  val regfile = new RegFileMonitorCfgIO
}


class MonitorIO()(implicit p: Parameters)
                  extends CoreBundle()(p)
{
  val trace = new TraceIO
  val cfg = new TraceCfgIO
}

class OwlInstruction()(implicit p: Parameters)
                       extends TracedInstruction()(p)
{
}

class TraceIO()(implicit p: Parameters)
                extends CoreBundle()(p)
{
  // What type of trace (should be compile time static?!)
  val kind = UInt(OUTPUT, 8)

  // True when a valid trace is on the wire, false otherwise.
  val valid = Bool(OUTPUT)

  val hartid = UInt(OUTPUT, hartIdLen)
  val insn = new OwlInstruction()(p)

  val time = UInt(OUTPUT, xLen)

  val register = UInt(OUTPUT, xLen) // Traced register

  val cfg = new TraceCfgIO

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
}

//object TraceSinkIO {
//  def apply() = new TraceIO.flip()
//}

abstract class TraceSource(implicit p: Parameters) extends CoreModule()(p) {
///  def io = _io
}

abstract class TraceSink(implicit p: Parameters) extends CoreModule()(p) {
//  def io = _io
}

abstract class OutTrace extends Bundle {
  // Keep it simple for now, we can do packing later....
  def check = {
    require(getWidth % 32 == 0, s"Width (${getWidth}) not multiple of 32.")
    require(getWidth <= OutTrace.MAX_SIZE, s"Width (${getWidth}) too big.")
  }

  val kind = UInt(width = OutTrace.KIND.SZ)
  //def width: UInt = size(kind)
  def is_wide: UInt = is_wide(kind)
  def size: UInt = size(kind)
  def lg2_size: UInt = lg2_size(kind) // TileLink size

  def toBits: UInt // toUInt reverses the order for some reason
}

object OutTrace
{
  // NB: Lower bit in KIND indicates 64-bit payload to simplify TileLink logic
  //def width(kind: UInt): UInt =
  //  Mux(!kind(0), UInt(32), UInt(64))
  def is_wide(kind: UInt): Bool = kind(0)
  def size(kind: UInt): UInt =
    Mux(!kind(0), UInt(32/8), UInt(64/8))
  def lg2_size(kind: UInt): UInt =
    Mux(!kind(0), UInt(log2Ceil(32/8)), UInt(log2Ceil(64/8)))

  object KIND
  {
    val SZ = 3          // Bits
    val UECALL    = 0x0 // Usermode ecall
    val RETURN    = 0x1 // Subsequent return from either Ecall or Interrupt
                        // Things will be recursive so the software side can
                        // figure out which is which. Example:
                        // user_ecall->super_ecall->interrupt-> ...
                        // ... interrupt_ret->super_ecall_ret->user_ecall_ret
    val SECALL    = 0x2 // Supervisor ecall
    val TIMESTAMP = 0x3 // Full 61-bit timestamp. Need to be inserted if time
                        // between traces are longer than a relative timestamp.
    val EXCEPTION = 0x4 // Non-ecall exception / interrupt
                        // Cause bits of an interrupt will be the irqnr
    val MSBPC     = 0x5 // Most significat bits of PC.
                        // Only injected if MSB bits of PC changed since last
                        // return trace from PRIV level
                        // Must follow directly after a return trace.
                        // Valid sequences:
                        //   [RETURN, MSBPC] or [RETURN, TIMESTAMP, MSBPC]
  }

  def MAX_SIZE = 64
}

class EcallTrace extends OutTrace
{
  val timestamp = UInt(width = 18)
  val regval = UInt(width = 11)

  def toBits = Cat(regval, timestamp, kind)
}
object EcallTrace
{
  def apply(trace: TraceIO, timeshift: UInt, uecall: Bool) = {
    val t = Wire(new EcallTrace)
    t.kind := Mux(uecall, UInt(OutTrace.KIND.UECALL), UInt(OutTrace.KIND.SECALL))
    t.timestamp := trace.time >> timeshift
    t.regval := trace.register
    t
  }
}

class ReturnTrace extends OutTrace
{
  val timestamp = UInt(width = 18)
  val regval = UInt(width = 11)
  val pc = UInt(width = 32)

  def toBits = Cat(pc, regval, timestamp, kind)
}
object ReturnTrace
{
  def apply(trace: TraceIO, timeshift: UInt) = {
    val t = Wire(new ReturnTrace)
    t.kind := UInt(OutTrace.KIND.RETURN)
    t.timestamp := trace.time >> timeshift
    t.regval := trace.register
    t.pc := trace.insn.iaddr
    t
  }
}

class MSBPCTrace extends OutTrace
{
  val timestamp = UInt(width = 18)
  val priv = UInt(width = PRV.SZ)
  val reserved = UInt(width = 9)
  val msb_pc = UInt(width = 32)

  def toBits = Cat(msb_pc, reserved, priv, timestamp, kind)
}
object MSBPCTrace
{
  def apply(trace: TraceIO, timeshift: UInt, pc_width: Int) = {
    val t        = Wire(new MSBPCTrace)
    t.kind      := UInt(OutTrace.KIND.MSBPC)
    t.timestamp := trace.time >> timeshift
    t.priv      := trace.insn.priv
    t.reserved  := 0.U
    // NB: vaddr is a signed integer so software needs to sign extend it
    t.msb_pc    := Cat(0.U((64 - pc_width) min 32),
                       trace.insn.iaddr(pc_width - 1, 32))
    t
  }
}

class ExceptionTrace extends OutTrace
{
  val timestamp = UInt(width = 18)
  val cause = UInt(width = 8 /* log2Ceil(1 + CSR.busErrorIntCause) */)
  val reserved = UInt(width = 3)

  def toBits = Cat(reserved, cause, timestamp, kind)

  require(CSR.busErrorIntCause == 128)

  def is_interrupt = cause(7).toBool
  def interrupt = cause(7,0)
}
object ExceptionTrace
{
  def apply(trace: TraceIO, timeshift: UInt, cause: UInt, interrupt: Bool) = {
    val t = Wire(new ExceptionTrace)
    t.kind := UInt(OutTrace.KIND.EXCEPTION)
    t.timestamp := trace.time >> timeshift
    t.reserved := UInt(0)
    // HACK: Interrupt bit not carried over from Rocket core cause bits in
    // TracedInstruction even though the highest bit is reserved for that
    // purpose???
    t.cause := Cat(interrupt.asUInt, cause(6, 0))
    t
  }
}

class TimestampTrace extends OutTrace
{
  val timestamp = UInt(width = 61)

  def toBits = Cat(timestamp, kind)
}
object TimestampTrace
{
  def apply(trace: TraceIO) = {
    val t = Wire(new TimestampTrace)
    t.kind := UInt(OutTrace.KIND.TIMESTAMP)
    t.timestamp := trace.time
    t
  }
}

class TraceLogicBundle()(implicit p: Parameters) extends CoreBundle()(p)
{
  val in = Input(new Bundle {
    val enable = Bool()
    val trace = new TraceIO
    val timeshift = UInt(width = 6) // 64
  })

  val out = new Bundle {
    val bits = UInt(width = OutTrace.MAX_SIZE)
    val valid = Bool()
    val debug = new TraceIO // For debug
  }

  require(OutTrace.MAX_SIZE == 64, "Review TraceLogicBundle")
}

class TraceLogic(implicit p: Parameters) extends CoreModule()(p)
{
  val DEBUG: Boolean = true
  val io = new TraceLogicBundle

  val insn = io.in.trace.insn
  val prev_priv      = RegEnable(insn.priv,      io.in.trace.valid)
  val prev_exception = RegEnable(insn.exception, io.in.trace.valid)
  val prev_interrupt = RegEnable(insn.interrupt, io.in.trace.valid)
  val prev_cause     = RegEnable(insn.cause,     io.in.trace.valid)

  def is_ecall(cause: UInt) = {
    // TODO: Need to add support for hypervisor calls.
    //require(!usingVM, "Need to add support for hypervisor ecalls")
    require(CSR.busErrorIntCause == 128)
    !cause(7) && cause(3) && !cause(2)
  }

  // Is this a transition from user to supervisor?
  // We could have used 'cause' but 'priv' saves 5 bits per comparison
  // NB: The code assumes that the debug bit, prev_priv(2), is low.
  val is_UtoS = prev_priv === UInt(PRV.U) && insn.priv === UInt(PRV.S)

  val trace_valid = Wire(Bool())
  val trace_bits  = Wire(UInt(width = OutTrace.MAX_SIZE))

  // We need a buffer for normal traces in case we need to inject
  // a timestamp
  val trace_fifo =
    Module(new Queue(UInt(width=OutTrace.MAX_SIZE), 1, flow=true))
  trace_fifo.io.enq.valid := trace_valid
  trace_fifo.io.enq.bits  := trace_bits

  trace_valid :=
    io.in.enable && RegNext(io.in.enable) &&
    io.in.trace.valid && prev_priv =/= insn.priv
  trace_bits :=
    Mux(prev_exception,
      Mux(is_ecall(prev_cause),
        // Normal ecall
        EcallTrace(io.in.trace, io.in.timeshift, is_UtoS).toBits,
        // Exception or interrupt
        ExceptionTrace(io.in.trace, io.in.timeshift, prev_cause,
                       prev_interrupt).toBits),
      // Return from ecall / exception / interrupt
      ReturnTrace(io.in.trace, io.in.timeshift).toBits)


  // MSBPC traces
  val reset_msbpc = !io.in.enable
  val msbpc = Mux(reset_msbpc, 0.U, insn.iaddr(coreMaxAddrBits-1, 32))
  val prev_user_msbpc_en  = Wire(init=Bool(false))
  val prev_super_msbpc_en = Wire(init=Bool(false))
  val inject_msbpc_trace  = Wire(init=Bool(false))
  prev_user_msbpc_en  := inject_msbpc_trace && insn.priv === UInt(PRV.U)
  prev_super_msbpc_en := inject_msbpc_trace && insn.priv === UInt(PRV.S)
  val prev_user_msbpc  = RegEnable(msbpc, reset_msbpc || prev_user_msbpc_en)
  val prev_super_msbpc = RegEnable(msbpc, reset_msbpc || prev_super_msbpc_en)
  val prev_msbpc_mux = Mux(insn.priv === UInt(PRV.U),
                           prev_user_msbpc, prev_super_msbpc)
  inject_msbpc_trace :=
    trace_valid && !prev_exception && prev_msbpc_mux =/= msbpc

  // We need a buffer for MSBPC traces since they have the lowest priority
  val msbpc_fifo =
    Module(new Queue(UInt(width=OutTrace.MAX_SIZE), 1, flow=true))
  msbpc_fifo.io.enq.valid := inject_msbpc_trace
  msbpc_fifo.io.enq.bits :=
    MSBPCTrace(io.in.trace, io.in.timeshift, coreMaxAddrBits).toBits


  // We need to inject timestamp traces since normal traces are only 18+ bits
  // of timestamp information.
  // TODO: Only inject timestamps when difference between two traces are
  // > 2^18 time. Note that io.in.trace.time is in a different clock domain
  // so we can't just use a simple counter.
  val this_time = io.in.trace.time(17, 0)
  val prev_time = RegNext(this_time)
  val time_wrap = prev_time === ((1 << 18) - 1).U && this_time === 0.U
  val inject_timestamp = Wire(init = Bool(false))
  val timestamp_bits = TimestampTrace(io.in.trace).toBits
  inject_timestamp := // On first enable "edge" or on wrap.
    io.in.enable && (RegNext(!io.in.enable) || time_wrap)

  // Select. Priority: timestamp > normal > msbpc
  val out_bits  = Mux(inject_timestamp,
                      timestamp_bits,
                      Mux(trace_fifo.io.deq.valid,
                          trace_fifo.io.deq.bits,
                          msbpc_fifo.io.deq.bits))
  val out_valid =
    io.in.enable &&
    (inject_timestamp || trace_fifo.io.deq.valid || msbpc_fifo.io.deq.valid)

  // Flow control FIFOs
  trace_fifo.io.deq.ready := !inject_timestamp
  msbpc_fifo.io.deq.ready := !inject_timestamp && !trace_fifo.io.deq.valid

  // Pipeline
  io.out.valid := RegNext(out_valid)
  io.out.bits  := RegNext(out_bits)
  io.out.debug := RegNext(io.in.trace)
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
