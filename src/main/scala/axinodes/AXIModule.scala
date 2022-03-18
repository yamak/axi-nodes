/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
package axinodes

import chisel3._

/**
 * This abstract class must be used instead of Chisel3 RawModule
 * for creating axi nodes. This class implicitly passes ioFactory to
 * axi node classes. Thanks to this mechanism, the user don't have to
 * create AXI IOs.
 */
abstract class AXIModule extends RawModule {

  class IOFactory
  {
    def apply[T<:Data](bundle:T)={
      IO(bundle)
    }
  }

  implicit val ioFactory:IOFactory = new IOFactory

  /**
   * This function create new clock and reset domain for new
   * node.
   * @param interface Type of node to be created
   * @return New Axi node
   * @example
   * val m0=AXIClockAndResetDomain(new AXI4LiteMasterNode with MasterLedController)
   */
  def AXIClockAndResetDomain[T<:AXIBaseInterface](interface: =>T):T={
    val aclock= IO(Input(Clock()))
    val areset= IO(Input(Bool()))
    withClockAndReset(aclock,!areset.asBool) {
      val i=interface
      aclock.suggestName(i.interfaceName+"_aclock")
      areset.suggestName(i.interfaceName+"_aresetn")
      i
    }
  }
}
