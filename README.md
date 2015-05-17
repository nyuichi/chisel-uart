UART for chisel
===============

Following classes are provided:

* UartTx
* UartRx
* Uart
* BufferedUartTx
* BufferedUartRx
* BufferedUart

In buffering mode, when the number of data coming from rxd signal exceeds the buffering capacity, uart modules discard the byte without any signals.
