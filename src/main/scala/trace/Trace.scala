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
  def prev_priv = RegEnable(priv, valid)
  def prev_exception = RegEnable(exception, valid)
  def prev_interrupt = RegEnable(interrupt, valid)
  def prev_cause = RegEnable(cause, valid)
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
    require(getWidth <= OutTrace.max_size, s"Width (${getWidth}) too big.")
  }

  val kind = UInt(width = OutTrace.KIND.SZ)
  def size: UInt = size(kind)
}

object OutTrace
{
  // NB: Lower bit in KIND indicates 64-bit payload to simplify TileLink logic
  def size(x: UInt): UInt = Mux(!x(0), UInt(32), UInt(64))

  object KIND
  {
    val SZ = 3          // Bits
    val UECALL    = 0x0 // Usermode ecall
    val SECALL    = 0x2 // SUpervisor ecall
    val EXCEPTION = 0x4 // Non-ecall exception / interrupt
                        // Cause bits of an interrupt will be the irqnr
    val TIMESTAMP = 0x6 // Full 61-bit timestamp. Need to be inserted if time
                        // between traces are longer than a relative timestamp.
    val RETURN    = 0x1 // Subsequent return from either Ecall or Interrupt
                        // Things will be recursive so the software side can
                        // figure out which is which. Example:
                        // user_ecall->super_ecall->interrupt-> ...
                        // ... interrupt_ret->super_ecall_ret->user_ecall_ret
  }

  def max_size = 64
}

class EcallTrace extends OutTrace {
  val timestamp = UInt(width = 18)
  val regval = UInt(width = 11)

  def apply(trace: TraceIO, timeshift: UInt, uecall: Bool) = {
    kind := Mux(uecall, UInt(OutTrace.KIND.UECALL), UInt(OutTrace.KIND.SECALL))
    timestamp := trace.time >> timeshift
    regval := trace.register
  }
}

class ReturnTrace extends OutTrace {
  val timestamp = UInt(width = 18)
  val regval = UInt(width = 11)
  val pc = UInt(width = 32)

  def apply(trace: TraceIO, timeshift: UInt) = {
    kind := UInt(OutTrace.KIND.RETURN)
    timestamp := trace.time >> timeshift
    regval := trace.register
    pc := trace.insn.iaddr
  }
}

class ExceptionTrace extends OutTrace {
  val timestamp = UInt(width = 21)
  val cause = UInt(width = 8 /* log2Ceil(1 + CSR.busErrorIntCause) */)

  require(CSR.busErrorIntCause == 128)

  def is_interrupt = cause(7).toBool
  def interrupt = cause(7,0)

  def apply(trace: TraceIO, timeshift: UInt, xcause: UInt) = {
    kind := UInt(OutTrace.KIND.EXCEPTION)
    timestamp := trace.time >> timeshift
    cause := xcause
  }
}

class TimestampTrace extends OutTrace {
  val timestamp = UInt(width = 29)
  // ??? TODO: should we timeshift?
  def apply(trace: TraceIO, timeshift: UInt) = {
    kind := UInt(OutTrace.KIND.TIMESTAMP)
    timestamp := trace.time >> timeshift
  }
}

class TraceLogicBundle()(implicit p: Parameters) extends CoreBundle()(p)
{
  val in = new Bundle {
    val trace = new TraceIO
    val timeshift = UInt(width = 6) // 64
  }

  val out = new Bundle {
    val bits = UInt(width = OutTrace.max_size)
    val valid = Bool()
  }
  require(OutTrace.max_size == 64, "Review TraceLogicBundle")
}

class TraceLogic(implicit p: Parameters) extends CoreModule()(p)
{
  val io = new TraceLogicBundle

  val insn = io.in.trace.insn
  val prev_priv = insn.prev_priv
  val prev_exception = insn.prev_exception
  val prev_interrupt = insn.prev_interrupt
  val prev_cause = insn.prev_cause

  val ecall = new EcallTrace
  val exception = new ExceptionTrace
  val returnn = new ReturnTrace

  def is_ecall(cause: UInt) = {
    // This also matches 'rocket.Causes.hypervisor_ecall' which we don't
    // support but will never hit, assuming we use default config with
    // usingVM = false.
    require(!usingVM, "Need to add support for hypervisor ecalls")
    require(CSR.busErrorIntCause == 128)
    !cause(7) && cause(3) && !cause(2)
  }

  // Is this a transition from user to supervisor?
  // We could have used 'cause' but 'priv' saves 5 bits per comparison
  // NB: The code assumes that the debug bit, prev_priv(2), is low.
  val is_UtoS = prev_priv === UInt(PRV.U) && insn.priv === UInt(PRV.S)

  val out_bits = UInt(width = OutTrace.max_size)
  when (prev_exception) {
    when (is_ecall(prev_cause)) {
      // Normal ecall
      // User to Supervisor or Supervisor to Machine
      ecall(io.in.trace, io.in.timeshift, is_UtoS)
      out_bits := ecall.asUInt
    } .otherwise {
      // Exception or interrupt
      exception(io.in.trace, io.in.timeshift, prev_cause)
      out_bits := exception.asUInt
    }
  } .otherwise {
    // Return from ecall / exception / interrupt
    returnn(io.in.trace, io.in.timeshift)
    out_bits := returnn.asUInt
  }

  // Pipeline
  io.out.bits  := RegNext(out_bits)
  io.out.valid := RegNext(io.in.trace.valid)
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
