// See LICENSE for license details.

package freechips.rocketchip.system

import freechips.rocketchip.config.{Config, Field}
import freechips.rocketchip.subsystem._

case object TraceSubsystemEnabled extends Field[Boolean](false)

class WithTraceSubsystem extends Config ((site, here, up) => {
  case TraceSubsystemEnabled => true
})

class TraceConfigBase extends Config(
  /* ??? Revisit: L2$ sits in front of DRAM controller,
   * we don't want aggregator log mem destination to be cached ... */
  new WithNL2CacheCapacity(0)
  ++ new WithTraceSubsystem)

class TraceConfigEmu extends Config(
  new TraceConfigBase
  ++ new WithNBigCores(1)
  ++ new LvNAConfigemu)

class TraceFPGAConfigzcu102 extends Config(
  new TraceConfigBase
  ++ new WithNBigCores(1)
  ++ new LvNAFPGAConfigzcu102)
