// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import Chisel._
import chisel3.{Input, Output}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._

object TraceCtrlConsts
{
  def config_offs      = 0x0000
  def buf_mask_offs    = 0x0008
  def status_offs      = 0x0100
  def buf_addr_offs    = 0x0500
  def buf_ptr_offs     = 0x9000
}

case class TraceCtrlParams(address: BigInt, nharts: Int)

class TraceCtrlOneBundle extends Bundle
{
  val in = new Bundle {
    val buf0_full = Input(Bool())
    val buf1_full = Input(Bool())
    val buf0_ptr = Input(UInt(32.W))
    val buf1_ptr = Input(UInt(32.W))
    val fifo_full = Input(Bool())
    val fifo_half = Input(Bool())
  }
  val out = new Bundle {
    val enable = Bool()
    val irq_en = Bool()
    val ignore_illegal_insn = Bool()
    val buf_mask = UInt(64.W)
    val buf0_addr = UInt(64.W)
    val buf0_full_clear = Bool()
    val buf0_ptr_clear = Bool()
    val buf1_addr = UInt(64.W)
    val buf1_full_clear = Bool()
    val buf1_ptr_clear = Bool()
  }
}

trait TraceCtrlBundle
{
  val params: TraceCtrlParams
  val harts = Vec(params.nharts, new TraceCtrlOneBundle())
}

trait TraceCtrlModule extends HasRegMap
/* with HasInterruptSources */
{
  val params: TraceCtrlParams
  val io: TraceCtrlBundle
  val interrupts: Vec[Bool]
  val addrWidth = 64 /* xLen */

  val enable = RegInit(UInt(0, width = 1))
  val irq_en = RegInit(UInt(0, width = 1))
  val ignore_illegal_insn = RegInit(UInt(0, width = 1))
  val (buf_mask, buf_mask_desc) =
    DescribedReg(UInt(addrWidth.W), "buf_mask", "Size of trace buffer minus 1.",
      reset=Some(0.U(addrWidth.W)), volatile=true)
  val fifo_full_irq_en = RegInit(UInt(0, width = 1))

  val buf0_addr = Reg(Vec(params.nharts, UInt(64.W)))
  val buf1_addr = Reg(Vec(params.nharts, UInt(64.W)))
  val buf0_full = Reg(Vec(params.nharts, UInt(0, width = 1)))
  val buf1_full = Reg(Vec(params.nharts, UInt(0, width = 1)))
  val buf0_full_clear = Wire(Vec(params.nharts, Bool(false)))
  val buf1_full_clear = Wire(Vec(params.nharts, Bool(false)))
  val buf0_ptr  = Reg(Vec(params.nharts, UInt(32.W)))
  val buf1_ptr  = Reg(Vec(params.nharts, UInt(32.W)))
  val buf0_ptr_clear = Wire(Vec(params.nharts, Bool(false)))
  val buf1_ptr_clear = Wire(Vec(params.nharts, Bool(false)))
  val fifo_full = Reg(Vec(params.nharts, UInt(0, width = 1)))
  val fifo_half = Reg(Vec(params.nharts, UInt(0, width = 1)))
  val fifo_full_clear = Wire(Vec(params.nharts, Bool(false)))
  val fifo_half_clear = Wire(Vec(params.nharts, Bool(false)))

  // Wire up inputs
  (buf0_full zip io.harts).foreach {
    case (b, a) => { b := a.in.buf0_full.asUInt }
  }
  (buf1_full zip io.harts).foreach {
    case (b, a) => { b := a.in.buf1_full.asUInt }
  }
  (buf0_ptr zip io.harts).foreach {
    case (b, a) => { b := a.in.buf0_ptr }
  }
  (buf1_ptr zip io.harts).foreach {
    case (b, a) => { b := a.in.buf1_ptr }
  }
  (fifo_full, fifo_full_clear, io.harts).zipped.foreach {
    case (f, clear, h) => {
      when      (h.in.fifo_full) { f := 1.U }
      .elsewhen (clear)          { f := 0.U }
    }
  }
  (fifo_half, fifo_half_clear, io.harts).zipped.foreach {
    case (f, clear, h) => {
      when      (h.in.fifo_half) { f := 1.U }
      .elsewhen (clear)          { f := 0.U }
    }
  }

  // Connect interrupts
  val any_buf_full : Bool =
    io.harts.exists { a => a.in.buf0_full || a.in.buf1_full }
  val any_fifo_full: Bool = io.harts.exists { _.in.fifo_full }
  interrupts := Vec.tabulate(1) { _ =>
    (enable &
      ((irq_en           & any_buf_full) |
       (fifo_full_irq_en & any_fifo_full))).toBool
  }

  def reg(r: UInt, gn: String, d: RegFieldDesc) =
    RegFieldGroup(gn, None, RegField.bytes(r, (r.getWidth + 7)/8, Some(d)))

  def ro_reg(r: UInt, gn: String, d: RegFieldDesc) =
    RegFieldGroup(gn, None, Seq(RegField.r(r.getWidth, RegReadFn { _ => (Bool(true), r) }, d)))

  def notify_reg(r: UInt, notify: Bool, gn: String, d: RegFieldDesc) =
    RegFieldGroup(gn, None, Seq(
      RegField(r.getWidth,
               RegReadFn { _ => (Bool(true), r) },
               RegWriteFn { (valid, data) =>
                 when (valid) { r := data }
                 notify := valid
                 Bool(true)
               },
               d)))

  val config_reg_fields = Seq(
    TraceCtrlConsts.config_offs -> Seq(
      RegField(1, enable,
        RegFieldDesc("enable",
                     "Enable: Set to one to enable tracing.",
                     reset = Some(0))),
      RegField(1, irq_en,
        RegFieldDesc("irq_en",
                     "Trace buffer full interrupt.",
                     reset = Some(0))),
      RegField(1, ignore_illegal_insn,
        RegFieldDesc("ignore_illegal_insn",
                     "Don't trace illegal instructions.",
                     reset = Some(0))),
      RegField(1, fifo_full_irq_en,
        RegFieldDesc("fifo_full_irq_en",
                     "FIFO half full interrupt.",
                     reset = Some(0)))),
    TraceCtrlConsts.buf_mask_offs -> reg(buf_mask, "buf_mask", buf_mask_desc))

  require(params.nharts <= 32,
          "FIXME: status bits will spill over into more than one register")
  val buf_full_fields = Seq.tabulate(params.nharts) { h => Seq(
    RegField(1,
      RegReadFn { _ => (Bool(true), buf0_full(h)) },
      RegWriteFn { (valid, clear) =>
        buf0_full_clear(h) := (valid & clear)
        Bool(true)
      },
      RegFieldDesc(s"buf0_full_hart${h}", s"Trace buffer0 full hart${h}.",
                   reset = Some(0), volatile=true)),
    RegField(1,
      RegReadFn { _ => (Bool(true), buf1_full(h)) },
      RegWriteFn { (valid, clear) =>
        buf1_full_clear(h) := (valid & clear)
        Bool(true)
      },
      RegFieldDesc(s"buf1_full_hart${h}", s"Trace buffer1 full hart${h}.",
                   reset = Some(0), volatile=true)),
    RegField(1,
      RegReadFn { _ => (Bool(true), fifo_half(h)) },
      RegWriteFn { (valid, clear) =>
        fifo_half_clear(h) := (valid & clear)
        Bool(true)
      },
      RegFieldDesc(s"fifo_half_hart${h}", s"Trace FIFO half full hart${h}.",
                   reset = Some(0), volatile=true)),
    RegField(1,
      RegReadFn { _ => (Bool(true), fifo_full(h)) },
      RegWriteFn { (valid, clear) =>
        fifo_full_clear(h) := (valid & clear)
        Bool(true)
      },
      RegFieldDesc(s"fifo_full_hart${h}", s"Trace FIFO full hart${h}.",
                   reset = Some(0), volatile=true)))
  }

  val status_reg_fields = Seq(
    TraceCtrlConsts.status_offs -> buf_full_fields.flatten
  )

  def buf_addr_reg_desc(h: Int, i: Int) =
    RegFieldDesc(
      name      = s"hart_${h}_buf${i}_addr",
      desc      = s"Trace buffer ${i} address for hart ${h}",
      group     = Some("buf_addr"),
      groupDesc = Some("Trace buffer address."),
      reset     = Some(BigInt(0)))

  def buf_addr_reg_offs(h: Int, i: Int) =
    TraceCtrlConsts.buf_addr_offs + h * 16 + i * 8
  val buf_addr_reg_fields = Seq.tabulate(params.nharts) { h => Seq(
      buf_addr_reg_offs(h, 0) ->
        notify_reg(buf0_addr(h), buf0_ptr_clear(h), s"buf0_addr_hart${h}",
                   buf_addr_reg_desc(h, 0)),
      buf_addr_reg_offs(h, 1) ->
        notify_reg(buf1_addr(h), buf1_ptr_clear(h), s"buf1_addr_hart${h}",
                   buf_addr_reg_desc(h, 1))
    )
  }

  def buf_ptr_reg_desc(h: Int, i: Int) =
    RegFieldDesc(
      name      = s"hart_${h}_buf${i}_ptr",
      desc      = s"Trace buffer ${i} pointer for hart ${h}",
      group     = Some("buf_ptr"),
      groupDesc = Some("Trace buffer pointer."),
      reset     = Some(BigInt(0)))
  def buf_ptr_reg_offs(h: Int, i: Int) =
    TraceCtrlConsts.buf_ptr_offs + h * 8 + i * 4
  val buf_ptr_reg_fields = Seq.tabulate(params.nharts) { h => Seq(
      buf_ptr_reg_offs(h, 0) -> ro_reg(buf0_ptr(h), "buf0_ptr", buf_ptr_reg_desc(h, 0)),
      buf_ptr_reg_offs(h, 1) -> ro_reg(buf1_ptr(h), "buf1_ptr", buf_ptr_reg_desc(h, 1))
    )
  }

  regmap((config_reg_fields ++ status_reg_fields ++
          buf_addr_reg_fields.flatten ++ buf_ptr_reg_fields.flatten):_*)

  // Pipeline outputs
  val enable_reg              = RegNext(enable.toBool)
  val irq_en_reg              = RegNext(irq_en.toBool)
  val ignore_illegal_insn_reg = RegNext(ignore_illegal_insn.toBool)
  val buf_mask_reg            = RegNext(buf_mask)

  // Shared outputs
  io.harts.foreach { a =>
    a.out.enable              := enable_reg
    a.out.irq_en              := irq_en_reg
    a.out.ignore_illegal_insn := ignore_illegal_insn_reg
    a.out.buf_mask            := buf_mask_reg
  }

  // Per hart
  (io.harts, buf0_addr, buf1_addr).zipped.foreach {
    case (a, b0a, b1a) => {
      a.out.buf0_addr := RegNext(b0a)
      a.out.buf1_addr := RegNext(b1a)
    }
  }
  (io.harts, buf0_full_clear, buf1_full_clear).zipped.foreach {
    case (a, b0fc, b1fc) => {
      a.out.buf0_full_clear := RegNext(b0fc)
      a.out.buf1_full_clear := RegNext(b1fc)
    }
  }
  (io.harts, buf0_ptr_clear, buf1_ptr_clear).zipped.foreach {
    case (a, clear0, clear1) => {
      a.out.buf0_ptr_clear := RegNext(clear0)
      a.out.buf1_ptr_clear := RegNext(clear1)
    }
  }
}

// Create a concrete TL2 version of the abstract TraceCtrl slave
class TLTraceCtrl(params: TraceCtrlParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "tracectrl", Seq("clemson,trace-ctrl"), interrupts = 1,
    size = 65536, beatBytes = 8)(
  new TLRegBundle(params, _)    with TraceCtrlBundle)(
  new TLRegModule(params, _, _) with TraceCtrlModule)

trait CanHaveTraceCtrl extends HasRocketTiles {
  this: BaseSubsystem =>

  val trace_ctrl_module =
    if (p(TraceSubsystemEnabled)) {
      Some(LazyModule(new TLTraceCtrl(TraceCtrlParams(0x50000000, p(NTiles)))))
    } else {
      None
    }

  if (p(TraceSubsystemEnabled)) {
    // Connect trace control device to system bus
    sbus.control_bus.toVariableWidthSlave(Some("trace_ctrl")) { trace_ctrl_module.get.node }
    ibus.fromSync := trace_ctrl_module.get.intnode
  }
}

trait CanHaveTraceCtrlModuleImpl extends HasRocketTilesModuleImp {
  val outer: CanHaveTraceCtrl

  if (p(TraceSubsystemEnabled)) {
    (outer.rocketTiles zip outer.trace_ctrl_module.get.module.io.harts).foreach {
      case (tile, hartio) => tile.module.ctrl_io := hartio
    }
  }
}
