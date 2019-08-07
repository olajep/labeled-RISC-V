package freechips.rocketchip.trace

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class RegFileIO(implicit p: Parameters) extends CoreBundle()(p) {
  val dst   = UInt(width = 5)
  val data  = UInt(width = xLen)
  val valid = Bool()
  val prv   = UInt(width = PRV.SZ)
}

class RegFileMonitorCfgIO extends Bundle {
  val regno_umode = UInt(width = 5) // Register to track for user mode
  val regno_smode = UInt(width = 5) // Register to track for supervisor mode
                                    // NB: Hypervisor left out
  val regno_mmode = UInt(width = 5) // Register to track for hypervisor mode
}

object RegFileMonitor {
  val DEBUG = false
  def connect(monitor: CoreMonitorIO) = {
    def valid(regno: UInt) = {
      monitor.regfile.valid && monitor.regfile.dst === regno
    }
    def trackreg(regno: UInt) = RegEnable(monitor.regfile.data, valid(regno))
    val reg_umode = trackreg(monitor.cfg.regfile.regno_umode)
    val reg_smode = trackreg(monitor.cfg.regfile.regno_smode)
    val reg_mmode = trackreg(monitor.cfg.regfile.regno_mmode)

    if (DEBUG) {
      when (valid(monitor.cfg.regfile.regno_umode)) {
        printf("RegFileMonitor: umode regno=[%d] val=[%x]\n",
          monitor.cfg.regfile.regno_umode, monitor.regfile.data)
      }
      when (valid(monitor.cfg.regfile.regno_smode)) {
        printf("RegFileMonitor: smode regno=[%d] val=[%x]\n",
          monitor.cfg.regfile.regno_smode, monitor.regfile.data)
      }
      when (valid(monitor.cfg.regfile.regno_mmode)) {
        printf("RegFileMonitor: mmode regno=[%d] val=[%x]\n",
          monitor.cfg.regfile.regno_mmode, monitor.regfile.data)
      }
    }

    def prvs = Seq(PRV.U,     PRV.S,     PRV.M)
    def outs = Seq(reg_umode, reg_smode, reg_mmode)
    def selects = prvs map (x => monitor.regfile.prv === UInt(x))
    monitor.trace.register := Mux1H(selects, outs)
  }
}

//class CoreTraceSourceIO(implicit p: Parameters) extends TraceSourceIO()(p) {
//  val regfile = new RegFileIO
//}

class CoreMonitorIO(implicit p: Parameters) extends MonitorIO()(p) {
  val regfile = new RegFileIO
}

object CoreMonitor {
  def connect(csr: CSRFile, monitor: CoreMonitorIO) = {
    val insn = csr.io.trace(0)
    monitor.trace.kind  := TraceKind.core
    monitor.trace.valid := insn.valid || insn.exception
    monitor.trace.insn  := insn // ??? := ???
    monitor.trace.time  := csr.io.time
    monitor.trace.hartid := csr.io.hartid

    monitor.regfile.prv := insn.priv
    RegFileMonitor.connect(monitor)

    // TODO: Don't hardcode
    monitor.cfg.regfile.regno_umode := Reg(init = UInt(10, 5)) // return value
    monitor.cfg.regfile.regno_smode := Reg(init = UInt(17, 5)) // syscall
    monitor.cfg.regfile.regno_mmode := Reg(init = UInt(17, 5)) // trapno
  }
}
