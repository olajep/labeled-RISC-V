package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreModule, RocketTile}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TraceAggregatorBundle(implicit p: Parameters) extends Bundle {
  val coremon = new MonitorIO().asInput
}

class TraceAggregatorParams(
  val ctrl_addr:  Int = 0x50000000,
  val fifo_depth: Int = 32)
{
}

// 64 bit packer
class Packer(implicit p: Parameters) extends Module
{
  val io = new Bundle {
    val in = new Bundle {
      val reset = Bool()
      val valid = Bool()
      val wide = Bool()
      val bits = UInt(width=64)
    }.asInput
    val out = new Bundle {
      val valid = Bool()
      val bits = UInt(width=64)
    }
  }

  val buf_valid = RegInit(Bool(false))
  val buf = Reg(UInt(width=32))

  when (io.in.reset) {
    buf_valid := false.B
  } .elsewhen (io.in.valid) {
    buf_valid := !io.in.wide ^ buf_valid
    buf := Mux(buf_valid, io.in.bits(63, 32), io.in.bits(31, 0))
  }

  val out_bits = Wire(UInt(width=64))
  val out_valid = Wire(init = Bool(false))
  when (!io.in.reset && io.in.valid) {
    out_valid := buf_valid || io.in.wide
    out_bits  := Mux(buf_valid, Cat(io.in.bits(31, 0), buf), io.in.bits)
  } . otherwise {
    out_valid := Bool(false)
  }

  // Pipeline
  io.out.valid := RegNext(out_valid)
  io.out.bits  := RegNext(out_bits)
}

trait HasTraceAggregatorTLLogic
{
  this: TraceAggregatorModule =>

  def connectTL() = {
    val (out, edge) = this.outer.node.out(0)

    val data_valid = Wire(Bool())
    val data_ready = Wire(Bool())

    val src      = Wire(UInt(0))
    val addr     = Wire(UInt(width=64))
    val data     = Wire(UInt(width=OutTrace.MAX_SIZE))
    val size     = log2Ceil(data.getWidth / 8)

    // Wire up FIFO dequeue flow
    data := this.fifo.io.deq.bits
    data_valid := this.fifo.io.deq.valid
    this.fifo.io.deq.ready := data_ready || this.flush

    // "Ring buffer 0"
    val trace_offset = RegInit(UInt(0, width=32))
    val trace_size_mask = this.ctrl.out.buf0_mask

    when (!this.enable) {
      trace_offset := 0.U
      this.tracebuf_full := false.B
    } .elsewhen (out.a.fire()) {
      val incr = (1 << size).U
      val new_traceoffset = (trace_offset + incr) & trace_size_mask
      trace_offset := new_traceoffset
      this.tracebuf_full := this.tracebuf_full || new_traceoffset === 0.U
    }
    this.ctrl.in.buf0_full := RegNext(this.tracebuf_full)

    // TODO: Require thatqueue.io.deq.bits buf0_addr must be aligned to
    // (trace_size_mask+1) so we can do | instead of +
    addr := this.ctrl.out.buf0_addr + trace_offset

    val (pflegal, pfbits) = edge.Put(src, addr, size.U, data.asUInt)

    val a_gen = Wire(init = Bool(false))
    a_gen := this.enable && !this.tracebuf_full && data_valid
    data_ready := out.a.fire() && data_valid

    out.a.bits := pfbits

    // Wire up flow control
    val (a_first, _, _) = edge.firstlast(out.a)
    val (_, _, d_done) = edge.firstlast(out.d)
    val in_flight = RegInit(Bool(false))
    in_flight := out.a.fire() || in_flight && !d_done
    out.a.valid := a_gen && pflegal && (!a_first || !in_flight)
    out.d.ready := Bool(true)

    // Tie off
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
    out.b.ready := Bool(true)

    if (this.DEBUG) {
      when (out.a.fire()) {
        printf("TraceAggregatorTLLogic: addr=[%x] offset=[%x] data=[%x]\n", addr, trace_offset, data)
      }
    }
  }
}

class TraceAggregatorModule(val outer: TraceAggregator)
                            extends LazyModuleImp(outer)
                            with HasTraceAggregatorTLLogic
{
  val DEBUG: Boolean = true
  val io = IO(new TraceAggregatorBundle)
  val ctrl: TraceCtrlBundle = outer.ctrl_module.module.io
  val tracebuf_full = Reg(init = Bool(false))

  val enable = Wire(Bool())
  val flush = Wire(Bool())
  enable := ctrl.out.enable
  flush := !enable || tracebuf_full

  // Pipeline:
  // coretrace --> TraceLogic --> Packer --> FIFO --> TileLink

  // Convert core trace to output trace format
  val outtrace = Module(new TraceLogic)
  outtrace.io.in.enable       := !flush
  outtrace.io.in.trace        := io.coremon.trace
  outtrace.io.in.trace.valid  := !flush && io.coremon.trace.valid
  outtrace.io.in.timeshift    := UInt(0)

  // Pack data in 64-bit chunks for TL write
  val packer = Module(new Packer)
  packer.io.in.reset := flush
  packer.io.in.valid := !flush && outtrace.io.out.valid
  packer.io.in.bits  := outtrace.io.out.bits
  packer.io.in.wide  := OutTrace.is_wide(packer.io.in.bits)

  // Outtrace buffer (for TileLink writeback)
  val fifo =
    Module(new Queue(UInt(width=OutTrace.MAX_SIZE), outer.params.fifo_depth))

  // Connect FIFO enq side
  // Silently drop entries if FIFO overflows
  fifo.io.enq.valid := !flush && packer.io.out.valid
  fifo.io.enq.bits  := packer.io.out.bits

  // Connect TileLink master
  // connectTL() will connect to the FIFO's dequeue side
  connectTL()

  if (DEBUG) {
    val t = outtrace.io.out.debug
    when (outtrace.io.out.valid) {
      printf("TraceAggregator: C%d: %d [%d]=[%x]=[%x] pc=[%x] priv=[%x] inst=[%x] " +
             "kind=[%x] " +
             "DASM(%x)\n",
             t.hartid, t.time(31,0), !t.insn.exception, t.insn.cause, t.insn.interrupt,
             t.insn.iaddr, t.insn.priv, t.insn.insn,
             outtrace.io.out.bits(2,0),
             t.insn.insn)
    }
  }
}

class TraceAggregator(tile: RocketTile, hartid: Int)(implicit p: Parameters)
                      extends LazyModule {
  val params = new TraceAggregatorParams
  val clientParams =
    TLClientParameters(
      name = s"trace_aggregator_${hartid}",
      sourceId = IdRange(0, 1))
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(clientParams))))
  val ctrl_module = LazyModule(
    new TLTraceCtrl(TraceCtrlParams(params.ctrl_addr + hartid * 4096)))

  lazy val module = new TraceAggregatorModule(this)
}
