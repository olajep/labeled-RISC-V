// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import Chisel._
import chisel3.{Input, Output}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

case class TraceCtrlParams(address: BigInt)

trait TraceCtrlBundle
{
  //val params: TraceCtrlParams
  val enable = Bool()
  val irq_en = Bool()
  val clock_shift = UInt(32.W)
  val buf0_addr = UInt(64.W)
  val buf0_mask = UInt(64.W)

  val buf0_full = Input(Bool())
}

trait TraceCtrlModule extends HasRegMap
/* with HasInterruptSources */
{
  val params: TraceCtrlParams
  val io: TraceCtrlBundle
  /* val interrupts: Vec[Bool] */
  val addrWidth = 64 /* xLen */

  val enable = RegInit(UInt(0, width = 1))
  val irq_en = RegInit(UInt(0, width = 1))
  val buf0_full = RegInit(UInt(0, width = 1))
  val (clock_shift, clock_shift_desc) =
    DescribedReg(UInt(32.W), "clock_shift", "Log 2 timestamp divider.",
      reset=Some(0.U(32.W)), volatile=true)
  val (buf0_addr, buf0_addr_desc) =
    DescribedReg(UInt(addrWidth.W), "buf0_addr", "Base address of trace buffer 0. Must have buf0_mask alignment.",
      reset=Some(0.U(addrWidth.W)), volatile=true)
  val (buf0_mask, buf0_mask_desc) =
    DescribedReg(UInt(addrWidth.W), "buf0_mask", "Size of trace buffer 0 minus 1.",
      reset=Some(0.U(addrWidth.W)), volatile=true)

  io.enable := enable.toBool
  io.clock_shift := clock_shift
  io.buf0_addr := buf0_addr
  io.buf0_mask := buf0_mask

  buf0_full := io.buf0_full.asUInt


  // TODO: Implement interrupts
  /* interrupts := Vec.tabulate(1) { i => false.B } */

  def reg(r: UInt, gn: String, d: RegFieldDesc) = RegFieldGroup(gn, None, RegField.bytes(r, (r.getWidth + 7)/8, Some(d)))
  regmap(
    0x00 -> Seq( /* config */
      RegField(1, enable,
        RegFieldDesc("enable", "Enable: Set to one to enable tracing.", reset = Some(0))),
      RegField(1, irq_en,
        RegFieldDesc("irq_en", "Trace buffer full interrupt.", reset = Some(0)))),
    0x04 -> Seq( /* status */
      RegField(1, buf0_full,
        RegFieldDesc("buf0_full", "Trace buffer0 full.", reset = Some(0)))),
    0x08 -> reg(clock_shift, "Log2 Clock Divider", clock_shift_desc),
    0x10 -> reg(buf0_addr, "buf0_addr", buf0_addr_desc),
    0x18 -> reg(buf0_mask, "buf0_mask", buf0_mask_desc)
  )
}

// Create a concrete TL2 version of the abstract TraceCtrl slave
class TLTraceCtrl(params: TraceCtrlParams)(implicit p: Parameters)
  extends TLRegisterRouter(params.address, "tracectrl", Seq("clemson,trace-ctrl"), 0 /* 1 interrupt */, beatBytes = 8)(
  new TLRegBundle(params, _)    with TraceCtrlBundle)(
  new TLRegModule(params, _, _) with TraceCtrlModule)
