/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
package examples

import chisel3._
import axinodes._
import chisel3.util._

/**
 * This is an simple AXI multiplier peripheral.
 * In this example, we have created slave and master node.
 * The Slave node receives multiplier, multiplicand and control signal
 * from outside. Then it calculates product.Master node gets this product
 * and transfer it to outside.
 */

/**
 * Slave interface implementation.
 */
trait SlaveInterfaceImp extends AXI4LiteSlaveInterface{

  lazy val regCount=3
  /*AXI register definitions.They must be define as lazy since regmap must be overrided as lazy*/
  private lazy val controlReg=RegInit(0.U(bitsWide.value.W))
  private lazy val multiplierReg=RegInit(0.U(bitsWide.value.W))
  private lazy val multiplicandReg=RegInit(0.U(bitsWide.value.W))

  val productReg=RegInit(0.U(bitsWide.value.W))
  val productReady=RegInit(false.B)

  lazy val regmap=AXI4LiteRegMap(0->AXI4LiteWriteReg(controlReg), 1->AXI4LiteWriteReg(multiplierReg),2->AXI4LiteWriteReg(multiplicandReg))//override regmap

  private val idle_state::active_state::Nil=Enum(2)
  private val stateReg=RegInit(idle_state)
  private val (counterVal,counterWrap)=Counter(stateReg===active_state,10) //Counter for dummy wait

  switch(stateReg){
    is(idle_state){
      when(controlReg(0)){
        productReady:=false.B
        stateReg:=active_state
      }
    }
    is(active_state){
      productReg:=multiplierReg*multiplicandReg
      wStall:=true.B //Stall AXI write channel.
      //Dummy wait for showing stall mechanism.This is not normally necessary.
      when(counterWrap)
      {
        wStall:=false.B
        productReady:=true.B
        stateReg:=idle_state
        controlReg:=0.U
      }
    }
  }

  def readProduct={
    productReady:=false.B
    productReg
  }
}

/**
 * Master node implementation
 */
trait MasterInterfaceImp extends AXI4LiteMasterInterface {

  val SLAVE_BASE_ADDR=0x44A00000

  def setProduct(product:UInt)={
    startWriteTransaction(product,SLAVE_BASE_ADDR.U)
  }
}
class SimpleAXIMultiplier extends AXIModule {
  val s=AXIClockAndResetDomain(new AXI4LiteSlaveNode with SlaveInterfaceImp) //Create slave node
  val m=AXIClockAndResetDomain(new AXI4LiteMasterNode with MasterInterfaceImp) // Create master node

  when(s.productReady){ //When slave node produces product
      m.setProduct(s.readProduct)
  }
}

object SimpleAXIMultiplier extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new SimpleAXIMultiplier,Array("--target-dir", "generated/")) // Generate verilog code
}