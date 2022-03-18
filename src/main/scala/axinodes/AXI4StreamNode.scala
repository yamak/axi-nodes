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
 * AXI4-Stream io bundle. AXI4Stream node uses this bundle for IO definition.
 * This bundle is used only for IO definition. This bundle is defined from
 * master's view point. [[AXI4StreamNode]] must use this bundle with Flipped.
 * [[AXI4StreamBundle]] is used for accessing AXI IOs.
 * @param addrWidth AXI address width
 * @param bitsWide AXI data bits wide.
 */
class VerilogAXI4StreamBundle(val bitsWide:AXI4DataBitsWide,val idWidth:Int,val destWidth:Int,val userWidth:Int) extends Bundle {
  val TVALID = Output(Bool())
  val TREADY = Input(Bool())
  val TDATA  = Output(UInt(bitsWide.value.W))
  val TSTRB  = Output(UInt((bitsWide.value/8).W))
  val TKEEP  = Output(UInt((bitsWide.value/8).W))
  val TLAST  = Output((Bool()))
  val TID  = Output(UInt(idWidth.W))
  val TDEST  = Output(UInt(destWidth.W))
  val TUSER  = Output(UInt(userWidth.W))
}

/**
 * AXI Stream signals bundle without valid and ready.
 * @param bitsWide TDATA width
 * @param idWidth TID width
 * @param destWidth TDEST width
 * @param userWidth TUSER width
 */
class RawAXI4StreamBundle(val bitsWide:AXI4DataBitsWide,val idWidth:Int,val destWidth:Int,val userWidth:Int) extends Bundle {
  val data = UInt(bitsWide.value.W)
  val strb = UInt((bitsWide.value/8).W)
  val keep = UInt((bitsWide.value/8).W)
  val last = Bool()
  val id   = UInt(idWidth.W)
  val dest = UInt(destWidth.W)
  val user = UInt(userWidth.W)
}

/**
 * AXI Stream signals bundle
 * @param bitsWide TDATA width
 * @param idWidth TID width
 * @param destWidth TDEST width
 * @param userWidth TUSER width
 */
class AXI4StreamBundle(val bitsWide:AXI4DataBitsWide,val idWidth:Int,val destWidth:Int,val userWidth:Int) extends Bundle {

  val t=Decoupled(new RawAXI4StreamBundle(bitsWide,idWidth,destWidth,userWidth))
}

/**
 * Companion object of AXI4StreamBundle
 */
object AXI4StreamBundle{

  /*DataView definition*/
  implicit val axiView = DataView[VerilogAXI4StreamBundle,AXI4StreamBundle](

    vab => new AXI4StreamBundle(vab.bitsWide,vab.idWidth,vab.destWidth,vab.userWidth),
    _.TVALID -> _.t.valid,
    _.TREADY -> _.t.ready,
    _.TDATA -> _.t.bits.data,
    _.TSTRB -> _.t.bits.strb,
    _.TKEEP -> _.t.bits.keep,
    _.TLAST -> _.t.bits.last,
    _.TID   -> _.t.bits.id,
    _.TDEST -> _.t.bits.dest,
    _.TUSER -> _.t.bits.user
  )
}

/**
 * User can create custom AXI4 Stream slave peripheral
 * by extending this trait.
 */
trait AXI4StreamSlaveInterface extends AXIBaseInterface{

  /**
   * This method returns TID width.
   * If you want to change TID width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TID width
   */
  protected def idWidth:Int=8

  /**
   * This method returns TDEST width.
   * If you want to change TID width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TDEST width
   */
  protected def destWidth:Int=4

  /**
   * This method returns TUSER width.
   * If you want to change TID width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TUSER width
   */
  protected def userWidth:Int=8

  protected val tStall:Bool
  protected val newDataReceived:Bool
  protected val axiStreamSignals:RawAXI4StreamBundle
}

/**
 * This class implements AXI4 Stream Slave logic.
 * The user can instantiate this class with [[AXI4StreamSlaveInterface]] trait
 * in a subclass of [[AXIModule]]
 * This class defines AXI IOs compatible with Xilinx naming.It increase
 * interface name at each instantiation automatically like following:
 * S00_AXIS_TVALID
 * S00_AXI_TREADY
 * S00_AXI_TDATA
 * ..
 * ..
 * ..
 * If user wants custom prefix, it should assign new name to
 * [[AXIBaseInterface.suggestedName]] during implementation of
 * [[AXI4StreamSlaveInterface]].This time the naming would be as follows:
 * FIFO_TVALID
 * FIFO_TREADY
 * FIFO_TDATA
 * ..
 * ..
 *
 * @param ioFactory This parameter is used for IO definition.
 * Because this class isn't subclass of Module, such a trick is used
 * @example
 * {{{
 * trait AXIFifo extends AXIStreamMasterInterface {
 *    ...
 *    ...
 * }
 * class AXI4FifoModule extends AXI4Module {
 *    val s0=AXIClockAndResetDomain(new AXI4StreamSlaveNode with AXIStreamFifo)
 * }
 * }}}
 */
abstract class AXI4StreamSlaveNode(implicit ioFactory:AXIModule#IOFactory) extends AXINode {
  this:AXI4StreamSlaveInterface=>

  //Implement interfaceName. If suggestedName is null(default value),
  //set prefix to SXX_AXIS otherwise set it to custom name
  protected[axinodes] val interfaceName = if (suggestedName == null) { //If there isn't a custom name
    val newName = "S" + "%02d".format(AXI4StreamSlaveNode.nameCounter) + "_AXIS"
    AXI4StreamSlaveNode.nameCounter += 1
    newName
  } else {
    suggestedName
  }

  /**
   * AXI IO definition
   */
  val axiIO=ioFactory(Flipped(new VerilogAXI4StreamBundle(bitsWide,idWidth,destWidth,userWidth))).suggestName(interfaceName) // Define AXI IO

  private val ioView = axiIO.viewAs[AXI4StreamBundle] // Get AXI IO view
  private val tReady=WireInit(false.B)

  private val tSkidBuffer=Module(new SkidBuffer(new RawAXI4StreamBundle(bitsWide,idWidth,destWidth,userWidth)))

  /**
   * The user can use this attribute for stalling write transaction.
   * AXI tready signal is kept low as long as this attribute is true.
   */
  override val tStall: Bool = WireInit(false.B)

  /**
   * When new data is received, this attribute becomes true.
   */
  override val newDataReceived: Bool = WireInit(false.B)

  /**
   * User can access current AXI Stream signals via this attribute
   */
  override val axiStreamSignals: RawAXI4StreamBundle = Wire(new RawAXI4StreamBundle(bitsWide, idWidth, destWidth, userWidth))

  tSkidBuffer.enq <> ioView.t
  tSkidBuffer.deq.ready:=tReady

  tReady:=tSkidBuffer.deq.valid && tSkidBuffer.deq.valid && (!ioView.t.valid || ioView.t.ready) && !tStall

  newDataReceived:=tReady
  axiStreamSignals<>tSkidBuffer.deq.bits
}

/**
 * Companion object of AXI4StreamSlaveNode class
 */
object AXI4StreamSlaveNode{

  private var nameCounter:Int=0
}

/**
 * User can create custom AXI4 Stream master peripheral
 * by extending this trait.
 */
trait AXI4StreamMasterInterface extends AXIBaseInterface{

  /**
   * This method returns TID width.
   * If you want to change TID width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TID width
   */
  protected def idWidth:Int=8

  /**
   * This method returns TDEST width.
   * If you want to change TDEST width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TDEST width
   */
  protected def destWidth:Int=4

  /**
   * This method returns TUSER width.
   * If you want to change TUSER width, you must override
   * this function as lazy val.
   * [[AXI4StreamSlaveNode]] uses this function.
   * @return TUSER width
   */
  protected def userWidth:Int=8

  protected def startWriteTransaction(data:UInt)
  protected def setKeep(keep:UInt)
  protected def setStrb(strb:UInt)
  protected def setLast(last:Bool)
  protected def setId(id:UInt)
  protected def setDest(dest:UInt)
  protected def setUser(user:UInt)
  protected def isReadyToWrite:Bool
}
/**
 * This class implements AXI4 Stream Master logic.
 * The user can instantiate this class with [[AXI4StreamMasterInterface]] trait
 * in a subclass of [[AXIModule]].
 * This class defines AXI IOs compatible with Xilinx naming.It increase
 * interface name at each instantiation automatically like following:
 * M00_AXI_TVALID
 * M00_AXI_TREADY
 * M00_AXI_TDATA
 * ..
 * ..
 * ..
 * If user wants custom prefix, it should assign new name to
 * [[AXIBaseInterface.suggestedName]] during implementation of
 * [[AXI4StreamMasterInterface]].This time, the naming would be as follows:
 * FIFO_TVALID
 * FIFO_TREADY
 * FIFO_TDATA
 * ..
 * ..
 *
 * @param ioFactory This parameter is used for IO definition.
 * Because this class isn't subclass of Module, such a trick is used
 * @example
 * {{{
 * trait AXIFifo extends AXI4StreamMasterInterface {
 *    ...
 *    ...
 * }
 * class AXIFifoModule extends AXI4Module {
 *    val s0=AXIClockAndResetDomain(new AXI4StreamMasterNode with AXIFifo)
 * }
 * }}}
 */
abstract class AXI4StreamMasterNode(implicit ioFactory:AXIModule#IOFactory) extends AXINode {

  this:AXI4StreamMasterInterface=>

  //Implement interfaceName. If suggestedName is null(default value),
  //set prefix to SXX_AXIS otherwise set it to custom name
  protected[axinodes] val interfaceName = if (suggestedName == null) { //If there isn't a custom name
    val newName = "M" + "%02d".format(AXI4StreamMasterNode.nameCounter) + "_AXIS"
    AXI4StreamMasterNode.nameCounter += 1
    newName
  } else {
    suggestedName
  }
  private val DEFAULT_KEEP = (-1.S((bitsWide.value/8).W)).asUInt

  /**
   * AXI IO definition
   */
  val axiIO=ioFactory(new VerilogAXI4StreamBundle(bitsWide,idWidth,destWidth,userWidth)).suggestName(interfaceName) // Define AXI IO
  private val ioView = axiIO.viewAs[AXI4StreamBundle] // Get AXI IO view

  private val startWriteReg=RegInit(false.B) //Write transaction start register
  private val axisSignalsReg=RegInit({
  val bundle =  Wire(new RawAXI4StreamBundle(bitsWide,idWidth,destWidth,userWidth))
    bundle.data:=0.U
    bundle.strb:=DEFAULT_KEEP
    bundle.keep:=DEFAULT_KEEP
    bundle.dest:=0.U
    bundle.id:=0.U
    bundle.last:=false.B
    bundle.user:=0.U
    bundle
  }
  )

  private val ready2writeReg=RegInit(true.B)
  private val tValidReg=RegInit(false.B)

  when(ioView.t.fire){
    ready2writeReg:=true.B
  }
  when(startWriteReg && !tValidReg){
    tValidReg:=true.B
    startWriteReg:=false.B //Set the start register to low
  }.elsewhen(ioView.t.ready){ // If the ready signal gets, set valid signal to low
    tValidReg:=false.B
  }

  ioView.t.bits <> axisSignalsReg
  ioView.t.valid:=tValidReg


  /**
   * This function starts new axi-lite write transaction.
   * @param data Data to be sent
   */
  override def startWriteTransaction(data: UInt): Unit = {
    axisSignalsReg.data:=data
    startWriteReg:=true.B
    ready2writeReg:=false.B
  }

  /**
   * Sets the value of TKEEP of the transaction
   */
  override protected def setKeep(keep: UInt): Unit = {
    axisSignalsReg.keep:=keep
  }

  /**
   * Sets the value of TSTRB of the transaction
   */
  override protected def setStrb(strb: UInt): Unit = {
    axisSignalsReg.strb:=strb
  }

  /**
   * Sets the value of TLAST of the transaction
   */
  override protected def setLast(last: Bool): Unit = {
    axisSignalsReg.last:=last
  }

  /**
   * Sets the value of TID of the transaction
   */
  override protected def setId(id: UInt): Unit = {
    axisSignalsReg.id:=id
  }

  /**
   * Sets the value of TDEST of the transaction
   */
  override protected def setDest(dest: UInt): Unit = {
    axisSignalsReg.dest:=dest
  }

  /**
   * Sets the value of TUSER of the transaction
   */
  override protected def setUser(user: UInt): Unit = {
    axisSignalsReg.user:=user
  }

  /**
   * This function indicates that whether master node can start new write
   * transaction.User can call [[startWriteTransaction]] only if the result of this
   * function is true.
   * @return Status
   */
  override protected def isReadyToWrite: Bool = ready2writeReg
}

/**
 * Companion object of AXI4StreamMasterNode
 */
object AXI4StreamMasterNode{

  private var nameCounter:Int=0
}