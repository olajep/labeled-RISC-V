// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.trace._

class TraceEmuTop(implicit p: Parameters) extends LvNAEmuTop
  with HasTraceCtrl
{
  override lazy val module = new TraceEmuTopModule(this)
}

class TraceEmuTopModule[+L <: TraceEmuTop](_outer: L) extends LvNAEmuTopModule(_outer)
  with HasTraceCtrlModuleImpl

class TraceFPGATop(implicit p: Parameters) extends LvNAFPGATop
  with HasTraceCtrl
{
  override lazy val module = new TraceFPGATopModule(this)
}

class TraceFPGATopModule[+L <: TraceFPGATop](_outer: L) extends LvNAFPGATopModule(_outer)
  with HasTraceCtrlModuleImpl

class TraceFPGATopAHB(implicit p: Parameters) extends LvNAFPGATopAHB

class TraceFPGATopAHBModule[+L <: TraceFPGATopAHB](_outer: L) extends LvNAFPGATopAHBModule(_outer)
