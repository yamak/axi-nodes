/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
package axinodes

import chisel3._
import chisel3.experimental.dataview._
import chisel3.util._

/**
 * AXI4-Lite io bundle. AXI4Lite node uses this bundle for IO definition.
 * This bundle is used only for IO definition. This bundle is defined from
 * master's view point. [[AXI4LiteSlaveNode]] must use this bundle with Flipped.
 * [[AXI4LiteBundle]] is used for accessing AXI IOs.
 * @param addrWidth AXI address width
 * @param bitsWide AXI data bits wide.
 */
class VerilogAXI4LiteBundle(val addrWidth:Int,val bitsWide:AXI4DataBitsWide) extends Bundle {
  val AWVALID = Output(Bool())
  val AWREADY = Input(Bool())
  val AWADDR  = Output(UInt(addrWidth.W))
  val AWPROT  = Output(UInt(3.W))

  val WVALID  = Output(Bool())
  val WREADY  = Input(Bool())
  val WDATA   = Output(UInt(bitsWide.value.W))
  val WSTRB   = Output(UInt((bitsWide.value/8).W))

  val BVALID  = Input(Bool())
  val BREADY  = Output(Bool())
  val BRESP   = Input(UInt(2.W))

  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())
  val ARADDR  = Output(UInt(addrWidth.W))
  val ARPROT  = Output(UInt(3.W))

  val RVALID = Input(Bool())
  val RREADY = Output(Bool())
  val RDATA  = Input(UInt(bitsWide.value.W))
  val RRESP  = Input(UInt(2.W))
}

/**
 * AXI4-Lite Read Data channel bundle
 * @param bitsWide Data bits wide
 */
class AXI4LiteReadDataChannel(val bitsWide:AXI4DataBitsWide) extends Bundle{
  require(bitsWide==AXI4DataBitsWide_32||bitsWide==AXI4DataBitsWide_64,"AXI4-Lite requires a fixed data bus width of either 32-bit or 64-bit")
  val data=UInt(bitsWide.value.W)
  val resp=UInt(2.W)
}

/**
 * AXI4-Lite Read Address Channel bundle
 * @param addrWidth Address width
 */
class AXI4LiteReadAddressChannel(val addrWidth:Int) extends Bundle{
  val addr=UInt(addrWidth.W)
  val prot=UInt(3.W)
}

/**
 * AXI4-Lite Write Data Channel bundle
 * @param bitsWide Data bits wide
 */
class AXI4LiteWriteDataChannel(val bitsWide:AXI4DataBitsWide) extends Bundle{
  require(bitsWide==AXI4DataBitsWide_32||bitsWide==AXI4DataBitsWide_64,"AXI4-Lite requires a fixed data bus width of either 32-bit or 64-bit")
  val data=UInt(bitsWide.value.W)
  val strb=UInt((bitsWide.value/8).W)
}

/**
 * AXI4-Lite Write Address Channel bundle
 * @param addrWidth Address width
 */
class AXI4LiteWriteAddressChannel(val addrWidth:Int) extends Bundle{
  val addr=UInt(addrWidth.W)
  val prot=UInt(3.W)
}

/**
 * AXI4-Lite Write Response Channel bundle
 */
class AXI4LiteWriteResponseChannel extends Bundle{
  val resp = UInt(2.W)
}

/**
 * AXI4-Lite bundle. This bundle is used for accessing
 * AXI IOs. This is used as DataView of VerilogAXI4LiteBundle
 * @param addrWidth Address width
 * @param bitsWide Data bits wide
 */
class AXI4LiteBundle(val addrWidth:Int,val bitsWide:AXI4DataBitsWide) extends Bundle{
  val aw = Decoupled(new AXI4LiteWriteAddressChannel(addrWidth))
  val w  = Decoupled(new AXI4LiteWriteDataChannel(bitsWide))
  val b  = Flipped(Decoupled(new AXI4LiteWriteResponseChannel))
  val ar = Decoupled(new AXI4LiteReadAddressChannel(addrWidth))
  val r  = Flipped(Decoupled(new AXI4LiteReadDataChannel(bitsWide)))
}

/**
 *  Companion object of AXI4LiteBundle
 */
object AXI4LiteBundle{

  /*DataView definition*/
  implicit val axiView = DataView[VerilogAXI4LiteBundle,AXI4LiteBundle](

    vab => new AXI4LiteBundle(vab.addrWidth,vab.bitsWide),

    _.AWVALID -> _.aw.valid,
    _.AWREADY -> _.aw.ready,
    _.AWADDR  -> _.aw.bits.addr,
    _.AWPROT  -> _.aw.bits.prot,

    _.WVALID  -> _.w.valid,
    _.WREADY  -> _.w.ready,
    _.WDATA   -> _.w.bits.data,
    _.WSTRB   -> _.w.bits.strb,

    _.BVALID  -> _.b.valid,
    _.BREADY  -> _.b.ready,
    _.BRESP   -> _.b.bits.resp,

    _.ARVALID -> _.ar.valid,
    _.ARREADY -> _.ar.ready,
    _.ARADDR  -> _.ar.bits.addr,
    _.ARPROT -> _.ar.bits.prot,

    _.RVALID -> _.r.valid,
    _.RREADY -> _.r.ready,
    _.RDATA  -> _.r.bits.data,
    _.RRESP ->  _.r.bits.resp
  )
}

/**
 * AXI4LiteReg Base class
 */
abstract class AXI4LiteReg

/**
 * AXI4Lite Slave Write Register Type. This
 * class is used as parameter of AXI4LiteRegMap
 * @param reg Mapped Register
 */
case class AXI4LiteWriteReg(reg:UInt) extends AXI4LiteReg

/**
 * AXI4Lite Slave Read Register type.This
 * class is used as parameter of AXI4LiteRegMap
 * @param reg Mapped Register
 */
case class AXI4LiteReadReg(reg:UInt) extends AXI4LiteReg

/**
 * This class stores AXI4Lite register mappings.
 * @param mapping Register mappings
 * @example {{{
 * AXI4LiteRegMap(0->AXI4LiteWriteReg(reg0),1->AXI4LiteWriteReg(reg1),2->AXI4LiteReadReg(reg2),3->AXI4LiteReadReg(reg3))
 * }}}
 */
case class AXI4LiteRegMap(mapping:(Int,AXI4LiteReg)*)

/**
 * User can create custom AXI4-Lite Slave peripheral
 * by extending this trait.
 */
trait AXI4LiteSlaveInterface extends AXIBaseInterface {
  /**
   * This method returns user defined register mappings.
   * [[AXI4LiteSlaveNode]] uses this function during writing
   * incoming AXI Writes transfers to user's registers.
   * The user must implement this method as lazy val
   * @return Register mappings
   */
  protected def regmap:AXI4LiteRegMap

  /**
   * This method returns user defined register count.
   * The user must implement this function as lazy val.
   * [[AXI4LiteSlaveNode]] uses this function.
   * @return Register count
   */
  protected def regCount:Int

  protected val wStall:Bool
  protected val rStall:Bool
  protected def currentWriteAddress:UInt
  protected def currentReadAddress:UInt
  protected val newDataReceived:Bool
}

/**
 * This class implements AXI4-Lite Slave logic.
 * The user can instantiate this class with [[AXI4LiteSlaveInterface]] trait
 * in a subclass of [[AXIModule]]
 * This class defines AXI IOs compatible with Xilinx naming.It increase
 * interface name at each instantiation automatically like following:
 * S00_AXI_AWVALID
 * S00_AXI_AWREADY
 * S00_AXI_AWADDR
 * ..
 * ..
 * ..
 * S00_AXI_BVALID
 * S01_AXI_BREADY
 * S01_AXI_BRESP
 *
 * If user wants custom prefix, it should assign new name to
 * [[AXIBaseInterface.suggestedName]] during implementation of
 * [[AXI4LiteSlaveInterface]].This time the naming would be as follows:
 * LEDCNTRL_AWVALID
 * LEDCNTRL_AWREADY
 * LEDCNTRL_AWADDR
 * ..
 * ..
 *
 * @param ioFactory This parameter is used for IO definition.
 * Because this class isn't subclass of Module, such a trick is used
 * @example
 * {{{
 * trait LedController extends AXI4LiteSlaveInterface {
 *    ...
 *    ...
 * }
 * class AXI4LiteSlaveLedController extends AXI4Module {
 *    val s0=AXIClockAndResetDomain(new AXI4LiteSlaveNode with LedController)
 * }
 * }}}
 */
abstract class AXI4LiteSlaveNode(implicit ioFactory:AXIModule#IOFactory) extends AXINode {

  this:AXI4LiteSlaveInterface=>
  require(regCount>0,"regCount must be bigger than 0")

  //Implement interfaceName. If suggested is null(default value),
  //set prefix to SXX_AXI otherwise set it to custom name
  protected[axinodes] val interfaceName = if (suggestedName == null) { //If there isn't a custom name
    val newName = "S" + "%02d".format(AXI4LiteSlaveNode.nameCounter) + "_AXI"
    AXI4LiteSlaveNode.nameCounter += 1
    newName
  } else {
    suggestedName
  }

  private val addrLSB = bitsWide.value/AXI4DataBitsWide_32.value+1 // If bitswide is 64, LSB of address data will be 3.If bitswide is 32, LSB of address data will be 2.

  private val addrBitWidth = if(regCount==1) 1 + addrLSB else log2Ceil(regCount) + addrLSB // Calculate address width
  private val addrMSB=addrBitWidth-1

  /**
   * AXI IO definition
   */
  val axiIO=ioFactory(Flipped(new VerilogAXI4LiteBundle(addrBitWidth, bitsWide))).suggestName(interfaceName) // Define AXI IO

  private val ioView = axiIO.viewAs[AXI4LiteBundle] // Get AXI IO view

  private val wrRegs = regmap.mapping.toList.filter(_._2.isInstanceOf[AXI4LiteWriteReg]).map(x => (x._1 -> x._2.asInstanceOf[AXI4LiteWriteReg])) //Get write registers of user
  private val rdRegs = regmap.mapping.toList.filter(_._2.isInstanceOf[AXI4LiteReadReg]).map(x => (x._1 -> x._2.asInstanceOf[AXI4LiteReadReg])) //Get read registers of user

  private val rdataReg = RegInit(0.U(ioView.r.bits.data.getWidth.W))

  private val wReady=WireInit(false.B)
  private val rReady=WireInit(false.B)

  private val rValidReg=RegInit(false.B)
  private val bValidReg=RegInit(false.B)

  /*Skid buffer is used for isolating read/valid interface of axi channels
  * Thanks to skid buffer, axi slave works with full throughput. Ready signals
  * are set high before valid signals.If slave is busy, data is registered and
  * ready signal of axi channel will set be low in the next cycle.
  * */
  private val awSkidBuffer=Module(new SkidBuffer(new AXI4LiteWriteAddressChannel(addrBitWidth)))
  private val wSkidBuffer=Module(new SkidBuffer(new AXI4LiteWriteDataChannel(bitsWide)))
  private val arSkidBuffer=Module(new SkidBuffer(new AXI4LiteReadAddressChannel(addrBitWidth)))

  /**
   * The user can use this attribute for stalling write transaction.
   * AXI wready and awready signal is kept low as long as this attribute is true.
   */
  override val wStall: Bool = WireInit(false.B)

  /**
   * The user can use this attribute for stalling read transaction.
   * AXI arready signal is kept low as long as this attribute is true.
   */
  override val rStall: Bool = WireInit(false.B)

  /**
   * When new data is received, this attribute becomes true and
   * incoming data is written to user's write registers.
   * So the user can read write-registers next rising edge of clock
   */
  override val newDataReceived: Bool = WireInit(false.B)

  awSkidBuffer.enq <> ioView.aw
  wSkidBuffer.enq <> ioView.w
  arSkidBuffer.enq <> ioView.ar

  awSkidBuffer.deq.ready:=wReady
  wSkidBuffer.deq.ready:=wReady
  arSkidBuffer.deq.ready:=rReady

  ioView.b.valid:=bValidReg // Set Write Response Valid
  ioView.b.bits.resp:=AXI4RWResponse_OKAY.value

  ioView.r.bits.data:=rdataReg // Set Read Data
  ioView.r.bits.resp:=AXI4RWResponse_OKAY.value
  ioView.r.valid:=rValidReg //Set Read Valid

  //If aw and w channels are valid and master don't stall write response channel and user doesn't stall write channel
  wReady:=awSkidBuffer.deq.valid && wSkidBuffer.deq.valid && (!ioView.b.valid || ioView.b.ready) && !wStall

  //If ar channel valid and slave or user doesn't stall read channel
  rReady:=arSkidBuffer.deq.valid && (!rValidReg || ioView.r.ready) && !rStall

  //Set new data received flag. Write registers will be ready in the next cycle
  newDataReceived:=wReady

  when(wReady){ //If we have valid data in deq channel of skid buffers
    axiWrite() //Write to user's write registers according to write strobe
    bValidReg:=true.B // Valid signal of write response channel will be high in the next cycle
  }.elsewhen(ioView.b.ready){ //If ready signal of write response channel is high after valid signal
    bValidReg:=false.B // Valid signal of write response channel will be low in the next cycle
  }

  when(rReady){ //If we have valid data in deq channel of skid buffers
    axiRead() // Read from user's read registers
    rValidReg:=true.B // Valid signal of read channel will be high in the next cycle
  }.elsewhen(ioView.r.ready){ //If ready signal of read channel is high after valid signal
    rValidReg:=false.B // Valid signal of read channel will be low in the next cycle
  }

  /**
   * AXI-Lite Write function. This function writes incoming data
   * to user's write registers according to write strobe.
   */
  private def axiWrite()={

    val subWords=WireInit(VecInit.tabulate(bitsWide.value/8)(_=>0.U(8.W))) //Data bytes
    for (elem <- wrRegs) { // Iterate over write registers
      elem._2.reg := elem._2.reg
      when(awSkidBuffer.deq.bits.addr(addrMSB,addrLSB) === elem._1.U) {
        for(byteIndex <- 0 until ioView.w.bits.strb.getWidth) { // Iterate over strobe bits
          when(wSkidBuffer.deq.bits.strb(byteIndex)===true.B) { // If strobe bit is high
            subWords(byteIndex):=wSkidBuffer.deq.bits.data((byteIndex + 1) * 8 - 1, byteIndex * 8) //Get corresponding byte from AXI Write channel
          }.otherwise{ // If strobe bit is high
            subWords(byteIndex):=elem._2.reg((byteIndex + 1) * 8 - 1, byteIndex * 8) // Don't touch to corresponding byte
          }
        }
        elem._2.reg:=subWords.asUInt // Assign sub-words to user register.
      }
    }
  }

  /**
   * AXI-Lite read function.This function reads user's
   * read registers
   */
  private def axiRead()={

    for (elem <- rdRegs) { // Iterate over user registers
      when(ioView.ar.bits.addr(addrMSB,addrLSB) === elem._1.U) { // If current address match with address of user register
        rdataReg:=elem._2.reg
      }
    }
  }

  /**
   * The user can use this function for getting current axi write address.
   * This function return master's awaddr directly.
   * @return Current Axi Write address
   */
  override def currentWriteAddress: UInt = awSkidBuffer.deq.bits.addr(addrMSB,addrLSB)

  /**
   * User can use this function for getting current axi read address.
   * This function return master's araddr directly.
   * @return Current Axi Write address
   */
  override def currentReadAddress: UInt = arSkidBuffer.deq.bits.addr(addrMSB,addrLSB)
}

/**
 * Companion object of AXI4LiteSlaveNode class
 */
object AXI4LiteSlaveNode
{
  private var nameCounter:Int=0 //Interface name prefix counter
}

/**
 * User can create custom AXI4-Lite Master peripheral
 * by extending this trait. All functions and attributes
 * in this trait is implemented by [[AXI4LiteMasterNode]]
 */
trait AXI4LiteMasterInterface extends AXIBaseInterface{

  private val defaultWriteStrobe:Int = if(bitsWide==AXI4DataBitsWide_32) 0x0F else 0xFF

  protected def startWriteTransaction(data:UInt,addr:UInt,strobe:UInt=defaultWriteStrobe.U)
  protected def startReadTransaction(addr:UInt)
  protected def isReadTransactionCompleted:Bool
  protected def currentReadData:UInt
  protected def isReadyToWrite:Bool
  protected def isReadyToRead:Bool
  protected val writeResponseError:UInt
  protected val readResponseError:UInt
}

/**
 * This class implements AXI4-Lite Master logic.
 * The user can instantiate this class with [[AXI4LiteMasterInterface]] trait
 * in a subclass of [[AXIModule]].
 * This class defines AXI IOs compatible with Xilinx naming.It increase
 * interface name at each instantiation automatically like following:
 * M00_AXI_AWVALID
 * M00_AXI_AWREADY
 * M00_AXI_AWADDR
 * ..
 * ..
 * ..
 * M00_AXI_BVALID
 * M01_AXI_BREADY
 * M01_AXI_BRESP
 *
 * If user wants custom prefix, it should assign new name to
 * [[AXIBaseInterface.suggestedName]] during implementation of
 * [[AXI4LiteMasterInterface]].This time, the naming would be as follows:
 * LEDCNTRL_AWVALID
 * LEDCNTRL_AWREADY
 * LEDCNTRL_AWADDR
 * ..
 * ..
 *
 * @param ioFactory This parameter is used for IO definition.
 * Because this class isn't subclass of Module, such a trick is used
 * @example
 * {{{
 * trait LedController extends AXI4LiteSlaveConfig {
 *    ...
 *    ...
 * }
 * class AXI4LiteMasterLedController extends AXI4Module {
 *    val s0=AXIClockAndResetDomain(new AXI4LiteMasterNode with LedController)
 * }
 * }}}
 */
abstract class AXI4LiteMasterNode(implicit ioFactory:AXIModule#IOFactory) extends AXINode
{
  this:AXI4LiteMasterInterface=>

  //Implement interfaceName. If suggested is null(default value),
  //set prefix to MXX_AXI otherwise set it to custom name
  val interfaceName = if (suggestedName == null) {
    val newName = "M" + "%02d".format(AXI4LiteMasterNode.nameCounter) + "_AXI"
    AXI4LiteMasterNode.nameCounter += 1
    newName
  } else {
    suggestedName
  }
  val axiIO=ioFactory(new VerilogAXI4LiteBundle(bitsWide.value, bitsWide)).suggestName(interfaceName) // Define AXI IO.Address and data width are same in master

  private val ioView = axiIO.viewAs[AXI4LiteBundle] // Get AXI IO view

  private val startWriteReg=RegInit(false.B) //Write transaction start register
  private val startReadReg=RegInit(false.B) //Read transaction start register

  private val wDataReg=RegInit(0.U(bitsWide.value.W))
  private val wStrobeReg=RegInit(0.U((bitsWide.value/8).W))
  private val wAddrReg=RegInit(0.U(bitsWide.value.W))
  private val wValidReg=RegInit(false.B)
  private val awValidReg=RegInit(false.B)

  private val bReadyReg=RegInit(false.B)

  private val rReadyReg=RegInit(false.B)

  private val arValidReg=RegInit(false.B)
  private val arAddrReg=RegInit(0.U(bitsWide.value.W))

  private val readDataReg=RegInit(0.U(bitsWide.value.W))

  private val ready2writeReg=RegInit(true.B)
  private val ready2ReadReg=RegInit(true.B)

  val readResponseError=WireInit(false.B)
  val writeResponseError=WireInit(false.B)

  //Assign registers to AXI IOs
  ioView.aw.valid:=awValidReg
  ioView.aw.bits.addr:=wAddrReg
  ioView.aw.bits.prot:=0.U

  ioView.w.valid:=wValidReg
  ioView.w.bits.strb:=wStrobeReg
  ioView.w.bits.data:=wDataReg

  ioView.b.ready:=bReadyReg


  ioView.r.ready:=rReadyReg
  ioView.ar.valid:=arValidReg
  ioView.ar.bits.addr:=arAddrReg
  ioView.ar.bits.prot:=0.U

  /*Write Transaction Logic*/

  //If write response valid and ready signal is high,
  //we are ready to new write transaction
  when(ioView.b.fire){
    ready2writeReg:=true.B
  }

  //If user has started a new transaction, wait until previous write response
  //signals are suitable.If ready signal is high but valid signal is low,
  //means the  slave has not sent write response yet.
  when(startWriteReg && !wValidReg&&(ioView.b.valid || !bReadyReg)){
    wValidReg:=true.B
    startWriteReg:=false.B //Set the start register to low
  }.elsewhen(ioView.w.ready){ // If the ready signal gets, set valid signal to low
    wValidReg:=false.B
  }

  //Same logic as above
  when(startWriteReg && !wValidReg&&(ioView.b.valid || !bReadyReg)){
    awValidReg:=true.B
  }.elsewhen(ioView.aw.ready){
    awValidReg:=false.B
  }

  //If user has started a new transaction or valid signal is high
  //(Just in case, If new transaction is started, we must finish it)
  when((startWriteReg||wValidReg) && !bReadyReg){
    bReadyReg:=true.B
  }.elsewhen(ioView.b.valid){
    bReadyReg:=false.B
  }

  writeResponseError:=bReadyReg&ioView.b.valid&ioView.b.bits.resp(1) //If bit 1 is high, there is an error

  /*Read Transaction Logic*/

  //If read valid and ready signal is high,
  //we are ready to new read transaction
  when(ioView.r.fire){
    ready2ReadReg:=true.B
  }

  /*Same logic as write signals*/
  when(startReadReg && !arValidReg&&(ioView.r.valid || !rReadyReg)){
    startReadReg:=false.B
    arValidReg:=true.B
  }.elsewhen(ioView.ar.ready){
    arValidReg:=false.B
  }

  /*Same logic as write response channel*/
  when((startReadReg || arValidReg) && !rReadyReg){
    rReadyReg:=true.B
  }.elsewhen(ioView.r.valid){
    readDataReg:=ioView.r.bits.data
    rReadyReg:=false.B
  }

  readResponseError:=rReadyReg&ioView.r.valid&ioView.r.bits.resp(1) //If bit 1 is high, there is an error

  /**
   * This function starts new axi-lite write transaction.
   * @param data Data to be sent
   * @param addr Slave register address
   * @param strobe AXI write strobe. It specify that which byte to be written.
   *               Each bit corresponds to one byte of data.
   */
  override def startWriteTransaction(data: UInt, addr:UInt,strobe: UInt): Unit = {
    wDataReg:=data
    wAddrReg:=addr
    wStrobeReg:=strobe
    startWriteReg:=true.B
    ready2writeReg:=false.B
  }

  /**
   * This function starts new axi-lite read transaction.
   * @param addr Slave register address to be read.
   */
  override def startReadTransaction(addr: UInt): Unit = {
    arAddrReg:=addr
    startReadReg:=true.B
    ready2ReadReg:=false.B
  }

  /**
   * This function indicates that whether read transaction
   * has been completed. User can read it in next rising edge
   * via [[currentReadData]].
   * @return Status
   */
  override def isReadTransactionCompleted: Bool = ioView.r.fire

  /**
   * This function returns current read data. If result of
   * [[isReadTransactionCompleted]] is true, user can read data
   * in the next rising edge via this function.
   * @return Current read data
   */
  override def currentReadData: UInt = readDataReg


  /**
   * This function indicates that whether master node can start new write
   * transaction.User can call [[startWriteTransaction]] only if the result of this
   * function is true.
   * @return Status
   */
  override def isReadyToWrite: Bool = ready2writeReg || ioView.b.fire

  /**
   * This function indicates that whether master node can start new read
   * transaction. User can call [[startReadTransaction]] only if the result of this
   * function is true.
   * @return Status
   */
  override def isReadyToRead: Bool = ready2ReadReg || ioView.r.fire
}

/**
 * Companion object of [[AXI4LiteMasterNode]]
 */
object AXI4LiteMasterNode
{
  private var nameCounter:Int=0
}
