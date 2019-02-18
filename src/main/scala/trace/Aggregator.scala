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

trait HasTraceAggregatorTLLogic
{
  this: TraceAggregatorModule =>

  def connectTL() = {
    val (out, edge) = this.outer.node.out(0)

    val data_valid = Wire(Bool())
    val data_ready = Wire(Bool())

    val src  = Wire(UInt(0))
    val addr = Wire(UInt(width=64))
    val data = Wire(new DefaultTraceFormat)
    val size = log2Ceil(data.getWidth / 8)

    // Wire up FIFO dequeue flow
    data := this.fifo.io.deq.bits
    data_valid := this.fifo.io.deq.valid
    this.fifo.io.deq.ready := data_ready

    // "Ring buffer 0"
    val trace_offset = RegInit(UInt(0, width=32))
    val trace_size_mask = this.ctrl.buf0_mask

    when (!this.ctrl.enable) {
      trace_offset := 0.U
      this.tracebuf_full := false.B
    } .elsewhen (out.a.fire()) {
      val incr = (1 << size).U
      val new_traceoffset = (trace_offset + incr) & trace_size_mask
      trace_offset := new_traceoffset
      this.tracebuf_full := this.tracebuf_full || new_traceoffset === 0.U
      printf("TraceAggregator: trace_offset=%x\n", trace_offset)
    }
    this.ctrl.buf0_full := this.tracebuf_full

    // TODO: Require thatqueue.io.deq.bits buf0_addr must be aligned to
    // (trace_size_mask+1) so we can do | instead of +
    addr := this.ctrl.buf0_addr + trace_offset

    val (pflegal, pfbits) = edge.Put(src, addr, size.U, data.asUInt)

    val a_gen = Wire(init = Bool(false))
    a_gen := this.ctrl.enable && !this.tracebuf_full && data_valid
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
  }
}

class TraceAggregatorModule(val outer: TraceAggregator)
                            extends LazyModuleImp(outer)
                            with HasTraceAggregatorTLLogic
{
  val DEBUG: Boolean = true
  val io = IO(new TraceAggregatorBundle)
  val ctrl: TraceCtrlBundle = outer.ctrl_module.module.io
  val tracebuf_full =  Reg(Bool())

  // Pipeline:
  // coretrace --> filter --> (coretrace->outtrace) --> fifo --> TileLink

  // Filter stage

  // TODO: flush fifos when ctrl.enable is 0
  val filter = Module(new FilterPrivSwitch(before=2, after=0))
  filter.io.in := io.coremon.trace
  filter.io.in.valid := ctrl.enable && io.coremon.trace.valid && !tracebuf_full

  // Outtrace buffer (for TileLink writeback)
  val fifo = Module(new Queue(new DefaultTraceFormat, outer.params.fifo_depth))

  // Convert core trace to output trace format
  val coretrace = RegEnable(filter.io.out, filter.io.out.valid)
  val outtrace = Wire(new DefaultTraceFormat)
  val outtrace_valid = RegNext(filter.io.out.valid)
  outtrace.register  := coretrace.register
  outtrace.timestamp := coretrace.time >> ctrl.clock_shift
  outtrace.priv      := coretrace.insn.priv

  // Connect FIFO enq side
  // Silently drop entries if FIFO overflows
  fifo.io.enq.valid := outtrace_valid
  fifo.io.enq.bits  := outtrace

  // Connect TileLink master
  connectTL()

  if (DEBUG) {
    when (outtrace_valid) {
      val t = coretrace
      printf("TraceAggregator: C%d: %d [%d]=[%x]=[%x] pc=[%x] priv=[%x] inst=[%x] " +
             "reg=[%x] time=[%d] priv=[%x] DASM(%x)\n",
             t.hartid, t.time(31,0), !t.insn.exception, t.insn.cause, t.insn.interrupt,
             t.insn.iaddr, t.insn.priv, t.insn.insn,
             outtrace.register, outtrace.timestamp, outtrace.priv,
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
