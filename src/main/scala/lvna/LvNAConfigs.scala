// See LICENSE for license details.

package freechips.rocketchip.system

import freechips.rocketchip.config.{Config, Field}
import freechips.rocketchip.subsystem._
import sifive.blocks.devices.uart._

case object UseEmu extends Field[Boolean](false)

class WithEmu extends Config ((site, here, up) => {
  case UseEmu => true
})

class WithEmuPeripheryUART extends Config ((site, here, up) => {
  case PeripheryUARTKey =>
    Seq(UARTParams(address = BigInt(0x60000000), withInterrupts = false,
                   withRx = false, withPrinter = true, withFast = true))
})

class LvNAConfigemu extends Config(
  new WithoutFPU
  ++ new WithNonblockingL1(8)
  ++ new WithNL2CacheCapacity(256)
  ++ new WithNBigCores(2)
  ++ new WithEmu
  ++ new WithRationalRocketTiles
  ++ new WithExtMemSize(0x800000L) // 8MB
  ++ new WithNoMMIOPort
  ++ new WithJtagDTM
  ++ new WithDebugSBA
  ++ new WithEmuPeripheryUART
  ++ new BaseConfig)

class LvNAFPGAConfigzedboard extends Config(
  new WithoutFPU
  ++ new WithNonblockingL1(8)
  ++ new WithNL2CacheCapacity(256)
  ++ new WithNZedboardCores(2)
  ++ new WithTimebase(BigInt(20000000)) // 20 MHz
  ++ new WithExtMemSize(0x100000000L)
  ++ new WithJtagDTM
  ++ new WithDebugSBA
  ++ new BaseFPGAConfig)

class LvNAFPGAConfigzcu102 extends Config(
  new WithoutFPU
  ++ new WithNonblockingL1(8)
  ++ new WithNL2CacheCapacity(2048)
  ++ new WithNBigCores(4)
  ++ new WithRationalRocketTiles
  ++ new WithTimebase(BigInt(100000000)) // 100 MHz
  ++ new WithExtMemSize(0x100000000L)
  ++ new WithNExtTopInterrupts(6)
  ++ new WithJtagDTM
  ++ new WithDebugSBA
  ++ new BaseFPGAConfig)

class LvNAFPGAConfigsidewinder extends Config(
  new WithoutFPU
  ++ new WithNonblockingL1(8)
  ++ new WithNL2CacheCapacity(2048)
  ++ new WithNBigCores(4)
  ++ new WithRationalRocketTiles
  ++ new WithTimebase(BigInt(10000000)) // 10 MHz
  ++ new WithExtMemSize(0x100000000L)
  ++ new WithJtagDTM
  ++ new WithDebugSBA
  ++ new BaseFPGAConfig)

class LvNAFPGAConfigrv32 extends Config(
  new WithoutFPU
  //++ new WithNonblockingL1(8)
  ++ new WithRV32
  ++ new WithNBigCores(1)
  ++ new WithRationalRocketTiles
  ++ new WithTimebase(BigInt(10000000)) // 10 MHz
  ++ new WithExtMemBase(0x80000000L)
  ++ new WithExtMemSize(0x80000000L)
  ++ new WithJtagDTM
  ++ new WithDebugSBA
  ++ new BaseFPGAConfig)
