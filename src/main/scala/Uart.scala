import Chisel._

class UartTx(val wtime: Int) extends Module {
  val io = new Bundle {
    val txd = UInt(OUTPUT, 1)
    val enq = Decoupled(UInt(width = 8)).flip;
  }
  val idle  = UInt(10, 4)
  val wtime_ = UInt(wtime, log2Up(wtime))

  val state = Reg(init = idle)
  val count = Reg(init = wtime_)
  val buf   = Reg(init = UInt("b111111111"))

  io.txd := buf(0)

  when (state === idle) {
    when (io.enq.valid) {
      buf := io.enq.bits ## UInt(0, 1)
      count := wtime_
      state := UInt(0)
    }
  } .otherwise {
    when (count === UInt(0)) {
      buf := UInt(1, 1) ## buf(8, 1)
      count := wtime_
      state := state + UInt(1)
    } .otherwise {
      count := count - UInt(1);
    }
  }

  io.enq.ready := state === UInt(10)
}

class UartRx(val wtime: Int) extends Module {
  val io = new Bundle {
    val rxd = UInt(INPUT, 1)
    val deq = Valid(UInt(width = 8))
  }
  val wtime_  = UInt(wtime, log2Up(wtime))
  val wtime_h = UInt(wtime / 2, log2Up(wtime)) // half period
  val idle = UInt(10, 4)

  val state = Reg(init = idle)
  val count = Reg(init = wtime_h)
  val buf   = Reg(init = UInt("b000000000"))
  val valid = Reg(init = Bool(false))

  when (state === idle) {
    when (io.rxd === UInt(0)) {
      when (count != UInt(0)) {
        count := count - UInt(1)
      } .otherwise {
        count := wtime_
        state := UInt(0)
        valid := Bool(false)
      }
    }
  } .elsewhen (state === UInt(9)) {
    when (count === wtime_h) {
      count := count - UInt(1)
      state := idle
      valid := Bool(true)
    } .otherwise {
      count := count - UInt(1)
    }
  } .otherwise {
    when (count === UInt(0)) {
      buf := io.rxd ## buf(8, 1)
      count := wtime_
      state := state + UInt(1)
    } .otherwise {
      count := count - UInt(1)
    }
  }

  io.deq.valid := valid
  io.deq.bits := buf(7, 0)
}

class Uart(val wtime: Int) extends Module {
  val io = new Bundle {
    val txd = UInt(OUTPUT, 1)
    val rxd = UInt(INPUT, 1)
    val enq = Decoupled(UInt(width = 8)).flip
    val deq = Valid(UInt(width = 8))
  }
  val tx = Module(new UartTx(wtime))
  val rx = Module(new UartRx(wtime))

  tx.io.txd <> io.txd
  tx.io.enq <> io.enq
  rx.io.rxd <> io.rxd
  rx.io.deq <> io.deq
}

class BufferedUartTx(val wtime: Int, val entries: Int) extends Module {
  val io = new Bundle {
    val txd = UInt(OUTPUT, 1)
    val enq = Decoupled(UInt(width = 8)).flip
    val count = UInt(OUTPUT, log2Up(entries + 1))
  }
  val queue = Module(new Queue(UInt(width = 8), entries))
  val tx = Module(new UartTx(wtime))

  queue.io.enq <> io.enq
  tx.io.enq <> queue.io.deq
  io.txd <> tx.io.txd
  io.count <> queue.io.count
}

class BufferedUartRx(val wtime: Int, val entries: Int) extends Module {
  val io = new Bundle {
    val rxd = UInt(INPUT, 1)
    val deq = Decoupled(UInt(width = 8))
    val count = UInt(OUTPUT, log2Up(entries + 1))
  }
  val queue = Module(new Queue(UInt(width = 8), entries))
  val rx = Module(new UartRx(wtime))

  queue.io.enq.bits := rx.io.deq.bits
  queue.io.enq.valid := rx.io.deq.valid
  io.deq <> queue.io.deq
  io.rxd <> rx.io.rxd
  io.count <> queue.io.count
}

class BufferedUart(val wtime: Int, val entries: Int) extends Module {
  val io = new Bundle {
    val txd = UInt(OUTPUT, 1)
    val rxd = UInt(INPUT, 1)
    val enq = Decoupled(UInt(width = 8)).flip
    val deq = Decoupled(UInt(width = 8))
  }
  val tx = Module(new BufferedUartTx(wtime, entries))
  val rx = Module(new BufferedUartRx(wtime, entries))

  tx.io.txd <> io.txd
  tx.io.enq <> io.enq
  rx.io.rxd <> io.rxd
  rx.io.deq <> io.deq
}

object Uart {

  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new UartLoopback)) { c =>
      new UartLoopbackTests(c)
    }
  }

  class UartLoopback extends Module {
    val io = new Bundle {
      val tx = Decoupled(UInt(width = 8)).flip
      val rx = Valid(UInt(width = 8))
    }
    val uart = Module(new BufferedUart(0x1ADB, 16))

    uart.io.rxd := uart.io.txd

    io.tx <> uart.io.enq
    io.rx <> uart.io.deq
  }

  class UartLoopbackTests(c: UartLoopback) extends Tester(c) {
    poke(c.io.tx.valid, 0)

    step(10)

    poke(c.io.tx.valid, 1)
    poke(c.io.tx.bits, 0xAA)

    do {
      step(1)
    } while (peek(c.io.rx.valid) == 0)

    expect(c.io.rx.valid, 1)
    expect(c.io.rx.bits, 0xAA)
  }

}
