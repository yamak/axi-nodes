package examples

import chisel3._
import chisel3.util._
import axinodes._

/**
 * This is an AXI Stream example.
 * In this example, an AXI Stream fifo has been created.
 * This fifo receives data from slave interface.
 * When it receives LAST signal, it sends buffered data from the master interface.
 */

/**
 * Slave interface implementation
 */
trait AXIStreamFifoWriteInterface extends AXI4StreamSlaveInterface {

  val data_received=AXI4UserIO(Output(UInt(1.W))) //It become high when TLAST is high
  val transfer_count=AXI4UserIO(Output(UInt(7.W)))
  val reset_byte_count=AXI4UserIO(Input(Bool()))

  val cnt=RegInit(0.U(7.W))
  val lastReg=RegInit(false.B)

  /**
   * Write data to queue
   * @param memio Queue enq interface
   */
  def write(memio:DecoupledIO[UInt]) {
    when(newDataReceived) {
      when(memio.ready) {
        memio.enq(axiStreamSignals.data)
        cnt:=cnt+1.U
      }
    }
  }
  when(axiStreamSignals.last){
    lastReg:=true.B
  }
  data_received:=lastReg
  transfer_count:=cnt

  when(reset_byte_count){ //Ready to new transfer
    lastReg:=false.B
    cnt:=0.U
  }
}

/**
 * Master interface implementation
 */
trait AXIStreamFifoReadInterface extends AXI4StreamMasterInterface{
  val rdAddr=RegInit(0.U(bitsWide.value.W))
  val startMemRead=RegInit(false.B)
  def read(memio:DecoupledIO[UInt]) {
      when(isReadyToWrite && memio.valid) {
        startWriteTransaction(memio.deq())
      }

  }
}

class AXIStreamFifo extends AXIModule{

  val aresetn= IO(Input(Bool()))
  val aclock= IO(Input(Clock()))

  val areset=WireInit(!aresetn) //Inverted reset because of AXI standard
  withClockAndReset(aclock,areset) { //Create clock and reset domain
    val enq = WireInit(0.U.asTypeOf(Decoupled(UInt(32.W)))) //Queue enq interface
    val deq = WireInit(0.U.asTypeOf(Decoupled(UInt(32.W)))) //Queue deq interface
    val queue = Module(new Queue(UInt(), 128, useSyncReadMem = true)) //Create Queue each element 32 bit
    queue.io.enq <> enq
    deq <> queue.io.deq
    val readEn = RegInit(false.B)

    val s = new AXI4StreamSlaveNode with AXIStreamFifoWriteInterface //Create slave node
    val m = new AXI4StreamMasterNode with AXIStreamFifoReadInterface //Create master node

    s.write(enq)

    when(s.lastReg) {
      readEn := true.B
    }
    when(deq.valid === false.B) {
      readEn := false.B
    }
    when(readEn) { // When TLAST is high
      m.read(deq) // Start AXI stream write transaction
    }
  }
}

object AXIStreamFifo extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AXIStreamFifo,Array("--target-dir", "generated/")) //Generate verilog code
}