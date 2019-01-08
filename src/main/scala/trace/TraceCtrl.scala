// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

case class TraceCtrlParams(address: BigInt)

trait TraceCtrlBundle
{
  val params: TraceCtrlParams
  val enable = Bool()
  val irq_en = Bool()
  val buf0_addr = UInt(64.W)
  val buf0_mask = UInt(64.W)

  val buf0_full = Bool()
}

trait TraceCtrlModule extends HasRegMap
{
  val params: TraceCtrlParams
  val io: TraceCtrlBundle
  val interrupts: Vec[Bool]
  val addrWidth = 64 /* xLen */

  val enable = RegInit(UInt(0, width = 1))
  val irq_en = RegInit(UInt(0, width = 1))
  val buf0_full = RegInit(UInt(0, width = 1))
  val (buf0_addr, buf0_addr_desc) =
    DescribedReg(UInt(addrWidth.W), "buf0_addr", "Base address of trace buffer 0.",
      reset=Some(0.U(addrWidth.W)), volatile=true)
  val (buf0_mask, buf0_mask_desc) =
    DescribedReg(UInt(addrWidth.W), "buf0_mask", "Log2 size of trace buffer 0.",
      reset=Some(0.U(addrWidth.W)), volatile=true)

  io.enable := enable.toBool
  io.buf0_addr := buf0_addr
  io.buf0_mask := buf0_mask


  // TODO: Implement interrupts
  interrupts := Vec.tabulate(1) { i => false.B }

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
    0x10 -> reg(buf0_addr, "buf0_addr", buf0_addr_desc),
    0x18 -> reg(buf0_mask, "buf0_mask", buf0_mask_desc)
  )
}

// Create a concrete TL2 version of the abstract TraceCtrl slave
class TLTraceCtrl(params: TraceCtrlParams)(implicit p: Parameters)
  extends TLRegisterRouter(params.address, "tracectrl", Seq("clemson,trace-ctrl"), 1, beatBytes = 8)(
  new TLRegBundle(params, _)    with TraceCtrlBundle)(
  new TLRegModule(params, _, _) with TraceCtrlModule)