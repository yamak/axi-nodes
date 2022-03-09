/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */


/**
 * This is an AXI4-Lite Master node example.
 * It writes values from 0 to 10 to sequential
 * addresses. It then reads back these written
 * values. You can test this module with Xilinx AXI
 * slave VIP with memory model.
 */

package examples

import chisel3._
import chisel3.util._
import axinodes._

/**
 * IO definition
 */
class MemRWBundle extends Bundle
{
  val wr_enable=Input(Bool())
  val rd_enable=Input(Bool())
  val wr_done=Output(Bool())
  val rd_done=Output(Bool())
  val readOut=Output(UInt(32.W))
}

/**
 * Implementation of our node.
 */
trait MemRW extends AXI4LiteMasterInterface {

  val SLAVE_BASE_ADDR=0x44A00000

  val io = AXI4UserIO(new MemRWBundle) // IO definition
  val rwPtr=RegInit(SLAVE_BASE_ADDR.U(32.W))
  val wDataReg=RegInit(0.U(8.W))
  val wrDoneReg=RegInit(false.B)
  val rdDoneReg=RegInit(false.B)
  val wrRisingEdge=io.wr_enable & !RegNext(io.wr_enable)
  val rdRisingEdge=io.rd_enable & !RegNext(io.rd_enable)
  val idle_state::write_state::read_state::Nil=Enum(3)
  val stateReg=RegInit(idle_state)

  switch(stateReg)
  {
    is(idle_state){
      when(wrRisingEdge){
        stateReg:=write_state
      }.elsewhen(rdRisingEdge){
        stateReg:=read_state
      }.otherwise{
        stateReg:=idle_state
      }
    }

    is(write_state)
    {
      wrDoneReg:=false.B
      when(isReadyToWrite){
        startWriteTransaction(wDataReg,rwPtr) // Start axi write transaction
        rwPtr:=rwPtr+4.U
        wDataReg:=wDataReg+1.U
      }
      when(rwPtr===(SLAVE_BASE_ADDR+4*10).U){
        rwPtr:=SLAVE_BASE_ADDR.U
        wDataReg:=0.U
        wrDoneReg:=true.B
        stateReg:=idle_state
      }
    }

    is(read_state){
      rdDoneReg:=false.B
      when(isReadyToRead){
        startReadTransaction(rwPtr)
        rwPtr:=rwPtr+4.U
      }
      when((rwPtr===(SLAVE_BASE_ADDR+4*10).U) && isReadTransactionCompleted){
        rwPtr:=SLAVE_BASE_ADDR.U
        rdDoneReg:=true.B
        stateReg:=idle_state
      }
    }
  }

  io.wr_done:=wrDoneReg
  io.rd_done:=rdDoneReg
  io.readOut:=currentReadData
}

class AXIMasterMemRW extends AXIModule{
  AXIClockAndResetDomain(new AXI4LiteMasterNode with MemRW) // Create AXI node with clock and reset domain.
}

object AXIMasterMemRW extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AXIMasterMemRW) //Generate verilog code
}
