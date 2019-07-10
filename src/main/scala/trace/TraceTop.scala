// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters

class TraceEmuTop(implicit p: Parameters) extends LvNAEmuTop
{
  override lazy val module = new TraceEmuTopModule(this)
}

class TraceEmuTopModule[+L <: TraceEmuTop](_outer: L) extends LvNAEmuTopModule(_outer)

class TraceFPGATop(implicit p: Parameters) extends LvNAFPGATop
{
  override lazy val module = new TraceFPGATopModule(this)
}

class TraceFPGATopModule[+L <: LvNAFPGATop](_outer: L) extends LvNAFPGATopModule(_outer)

class TraceFPGATopAHB(implicit p: Parameters) extends LvNAFPGATopAHB

class TraceFPGATopAHBModule[+L <: LvNAFPGATopAHB](_outer: L) extends LvNAFPGATopAHBModule(_outer)
