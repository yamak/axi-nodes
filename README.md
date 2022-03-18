# axi-nodes

This repository provide Chisel3 libraries to creation AXI modules. 
With this library, you can easily develop AXI modules in Chisel3. 

The purpose of writing this library is that I want to quickly create AXI peripherals during the Chisel learning process. At first I thought of using Rocket-Chip's AMBA library, but realized that it is difficult to design standalone AXI peripherals with this library. 

## Features

* AXI core is designed to be able to provide %100 throughput on both read and write channel. In my first design, slave core was not working with full throughput. But after some time on [Daniel E Gisselquist's blog](https://zipcpu.com/), I realized that we can design better. Since skid buffer is used in this version, the slave core can run at full throughput.
  
* Automatic name generation. Axi signal names are automatically generated in accordance with Xilinx's naming.

* Well documented. 

* For now, AXI4-FULL not yet been developed


## Examples

### SimpleGpioController
A simple AXI GPIO Controller with 8-bit input and output ports

### MasterMemRW
This is an AXI4-Lite Master node example.
It writes values from 0 to 10 to sequential addresses. It then reads back these written values. You can test this module with Xilinx AXI slave VIP with memory model.

### SimpleAXIMultiplier
Multiplier peripheral. It has AXI4-Lite slave and master node. The Slave node receives multiplier, multiplicand and control signal from outside. Then it calculates product.Master node gets this product and transfer it to outside.

### AXIStreamFifo
In this example, an AXI Stream fifo has been created.This fifo receives data from slave interface. 
When it receives LAST signal, it sends buffered data from the master interface.