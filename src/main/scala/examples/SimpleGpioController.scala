/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
package examples

import chisel3._
import axinodes._

/**
 * This is an AXI4-Lite Slave example.
 * In this example, we have created a simple
 * AXI GPIO Controller. It have 8 bit input and
 * output ports.
 */


class GpioControllerBundle extends Bundle
{
  val outputs=Output(UInt(8.W))
  val inputs=Input(UInt(8.W))
}

trait SimpleGpioController extends AXI4LiteSlaveInterface {

  val io = AXI4UserIO(new GpioControllerBundle)
  lazy val regCount=4 // override regCount
  /*AXI register definition. They must be define as lazy since regmap must be overrided as lazy*/
  lazy val inputReg=RegInit(0.U(32.W))
  lazy val outputReg=RegInit(0.U(32.W))
  lazy val regmap=AXI4LiteRegMap(0->AXI4LiteWriteReg(outputReg),1->AXI4LiteReadReg(inputReg)) // Override regmap.It must be overrided as lazy.
  inputReg:=io.inputs
  io.outputs:=outputReg(8,0)
}

class AXISimpleGpioController extends AXIModule{
  AXIClockAndResetDomain(new AXI4LiteSlaveNode with SimpleGpioController)
}

object AXISimpleGpioController extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AXISimpleGpioController,Array("--target-dir", "generated/"))
}