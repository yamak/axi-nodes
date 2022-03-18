/*
 * Copyright 2022 Yusuf YAMAK
 *
 * SPDX-License-Identifier: MIT
 */
import chisel3._

package object axinodes {
  /**
   * AXI data bits wide enumerations
   */
  sealed trait AXI4DataBitsWide{val value:Int}
  case object AXI4DataBitsWide_8 extends AXI4DataBitsWide {val value=8}
  case object AXI4DataBitsWide_16 extends AXI4DataBitsWide{val value=16}
  case object AXI4DataBitsWide_32 extends AXI4DataBitsWide{val value=32}
  case object AXI4DataBitsWide_64 extends AXI4DataBitsWide{val value=64}
  case object AXI4DataBitsWide_128 extends AXI4DataBitsWide{val value=128}
  case object AXI4DataBitsWide_256 extends AXI4DataBitsWide{val value=256}
  case object AXI4DataBitsWide_512 extends AXI4DataBitsWide{val value=512}
  case object AXI4DataBitsWide_1024 extends AXI4DataBitsWide{val value=1024}

  /**
   * Axi response enumerations
   */
  sealed trait AXI4RWResponse{val value:UInt}
  case object AXI4RWResponse_OKAY extends AXI4RWResponse{val value=0.U}
  case object AXI4RWResponse_EXOKAY extends AXI4RWResponse{val value=1.U}
  case object AXI4RWResponse_SLVERR extends AXI4RWResponse{val value=2.U}
  case object AXI4RWResponse_DECERR extends AXI4RWResponse{val value=3.U}

  /**
   * Base axi interface
   */
  trait AXIBaseInterface{
    protected[axinodes] val interfaceName:String //This attributes holds axi node prefix name

    /**
     * User can determine prefix of axi interface name via this attribute.
     * If user assign a string to this attribute, io names will be as follows
     * LEDCNTRL_AWVALID
     * LEDCNTRL_AWREADY
     * LEDCNTRL_AWADDR
     *
     * If this attribute is not assigned, io names will be as follows
     * M00_AWVALID
     * ...
     * ...
     * M01_AWVALID
     * ...
     * ...
     *
     * S00_WVALID
     * ...
     * ...
     * S01_WVALID
     * ...
     * ...
     *
     */
    protected def suggestedName:String=null

    /**
     * Bits wide of axi interface
     */
    protected def bitsWide:AXI4DataBitsWide=AXI4DataBitsWide_32

    protected def AXI4UserIO[T<:Data](gen:T):T
  }

  abstract class AXINode(implicit val ioFactory:AXIModule#IOFactory){


    /**
     * Thanks to this function, although the implementation of
     * AXI4LiteSlaveConfig is not a subclass of the Module,
     * user can can define IO via this function.
     * @param bundle Bundle that will be IO
     * @tparam T
     * @return bundle parameter
     */
    def AXI4UserIO[T<:Data](bundle: T):T={
      ioFactory(bundle)
    }
  }


}
