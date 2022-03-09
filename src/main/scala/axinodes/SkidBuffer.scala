/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
package axinodes

import chisel3._
import chisel3.util._

/**
 * Skid buffer implementation.
 * @param gen Type of data to be wrapped in SkidBuffer
 */
class SkidBuffer[T <: Data](gen: => T) extends Module {
  val enq=IO(Flipped(EnqIO(gen)))
  val deq=IO(Flipped(DeqIO(gen)))

  private val validReg=RegInit(false.B)
  private val dataReg=RegInit(0.U.asTypeOf(gen))

  enq.ready := !validReg
  deq.valid := !reset.asBool && (enq.valid || validReg)

  when(enq.fire && (deq.valid && !deq.ready)){ //Input data valid, but the output is not ready
    validReg:=true.B
  }.elsewhen(deq.ready){ // The output is ready
    validReg:=false.B //Bypass input
  }

  when(!deq.valid || deq.ready){ // The output is ready, but input isn't valid
    dataReg:=0.U.asTypeOf(gen)
  }.elsewhen(enq.fire){ // There is incoming data
    dataReg:=enq.bits //Register incoming data
  }

  when(validReg){ // If validReg is true, assign registered data to output
    deq.bits:=dataReg
  }.elsewhen(enq.valid){ //If validReg is false,enq.ready is true.So bypass input to output
    deq.bits:=enq.bits
  }.otherwise{//If there is no data
    deq.bits:=0.U.asTypeOf(gen)
  }

}
