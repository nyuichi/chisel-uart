package aqua.uart

import Chisel._

class UartTx(val wtime: Int) extends Module {
  val io = new Bundle {
    val txd = Bool(OUTPUT)
    val enq = Decoupled(UInt(width = 8)).flip
  }
  val time = UInt(wtime, log2Up(wtime))
  val idle :: runnings  = Enum(UInt(), 11)

  val state = Reg(init = idle)
  val count = Reg(init = time)
  val buf   = Reg(init = UInt("b111111111"))

  io.txd := buf(0)

  switch (state) {
    is(idle) {
      when (io.enq.valid) {
        buf := io.enq.bits ## UInt("b0")
        count := time
        state := runnings.last
      }
    }
    is(runnings) {
      when (count === UInt(0)) {
        buf := UInt("b1") ## buf(8, 1)
        count := time
        state := state - UInt(1)
      } .otherwise {
        count := count - UInt(1)
      }
    }
  }

  io.enq.ready := (state === idle)
}

class UartRx(val wtime: Int) extends Module {
  val io = new Bundle {
    val rxd = Bool(INPUT)
    val deq = Decoupled(UInt(width = 8))
  }
  val time = UInt(wtime, log2Up(wtime))
  val time_h = UInt(wtime / 2, log2Up(wtime)) // half period
  val idle :: stop :: runnings = Enum(UInt(), 11)

  val state = Reg(init = idle)
  val count = Reg(init = time_h)
  val buf   = Reg(init = UInt("b000000000"))
  val valid = Reg(init = Bool(false))

  when (valid && io.deq.ready) {
    valid := Bool(false)
  }

  switch (state) {
    is(idle) {
      when (io.rxd === UInt(0)) {
        when (count != UInt(0)) {
          count := count - UInt(1)
        } .otherwise {
          count := time
          state := runnings.last
          valid := Bool(false)
        }
      }
    }
    is(runnings) {
      when (count === UInt(0)) {
        buf := io.rxd ## buf(8, 1)
        count := time
        state := state - UInt(1)
      } .otherwise {
        count := count - UInt(1)
      }
    }
    is(stop) {
      when (count === time_h) {
        count := count - UInt(1)
        state := idle
        valid := Bool(true)
      } .otherwise {
        count := count - UInt(1)
      }
    }
  }

  io.deq.valid := valid
  io.deq.bits := buf(7, 0)
}

class UartIO() extends Bundle {
  val enq = Decoupled(UInt(width = 8))
  val deq = Decoupled(UInt(width = 8)).flip
}

class UartPeripheral extends Bundle {
  val txd = Bool(OUTPUT)
  val rxd = Bool(INPUT)
}

class Uart(val wtime: Int) extends Module {
  val io = new Bundle {
    val ctl = (new UartIO).flip
    val pins = new UartPeripheral
  }
  val tx = Module(new UartTx(wtime))
  val rx = Module(new UartRx(wtime))

  tx.io.txd <> io.pins.txd
  rx.io.rxd <> io.pins.rxd
  tx.io.enq <> io.ctl.enq
  rx.io.deq <> io.ctl.deq
}

class BufferedUartTx(val wtime: Int, val entries: Int) extends Module {
  val io = new Bundle {
    val txd = Bool(OUTPUT)
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
    val rxd = Bool(INPUT)
    val deq = Decoupled(UInt(width = 8))
    val count = UInt(OUTPUT, log2Up(entries + 1))
  }
  val queue = Module(new Queue(UInt(width = 8), entries))
  val rx = Module(new UartRx(wtime))

  queue.io.enq <> rx.io.deq
  io.deq <> queue.io.deq
  io.rxd <> rx.io.rxd
  io.count <> queue.io.count
}

class BufferedUart(val wtime: Int, val entries: Int) extends Module {
  val io = new Bundle{
    val ctl = (new UartIO).flip
    val pins = new UartPeripheral
  }
  val tx = Module(new BufferedUartTx(wtime, entries))
  val rx = Module(new BufferedUartRx(wtime, entries))

  tx.io.txd <> io.pins.txd
  rx.io.rxd <> io.pins.rxd
  tx.io.enq <> io.ctl.enq
  rx.io.deq <> io.ctl.deq
}

class DummyUart(val input: Array[Int]) extends Module {
  val io = (new UartIO).flip

  val arr = Vec(input.map(x => UInt(x, width=8)))
  val idx = RegInit(UInt(0, width = log2Up(input.size + 1)))

  println("input: ", input.size)

  io.enq.ready := Bool(true)
  when (io.enq.valid) {
    printf("write to DummyUart: %d\n", io.enq.bits)
  }

  val obuf = Reg(Valid(UInt(width = 8)))
  io.deq.valid := obuf.valid
  io.deq.bits  := obuf.bits

  if (input.isEmpty) {
    obuf.bits := UInt(0)
    obuf.valid := Bool(false)

  } else {
    when (io.deq.ready && idx < UInt(input.size)) {
      obuf.bits := arr(idx)
      obuf.valid := Bool(true)
      printf("read from DummyUart : %d\n", arr(idx))
      idx  := idx + UInt(1)
    } .otherwise {
      obuf.bits := UInt(0)
      obuf.valid := Bool(false)
    }
  }
}

object Uart {

  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new UartLoopback)) { c =>
      new UartLoopbackTests(c)
    }
    chiselMainTest(args, () => Module(new UartBufferedLoopback)) { c =>
      new UartBufferedLoopbackTests(c)
    }
    chiselMainTest(args, () => Module(new DummyUart(Array(0xba, 0xad, 0xf0, 0x0d)))) { c =>
      new DummyUartTests(c)
    }
  }

  class UartLoopback extends Module {
    val io = (new UartIO).flip
    val uart = Module(new Uart(0x1ADB))

    uart.io.pins.rxd := uart.io.pins.txd

    io.enq <> uart.io.ctl.enq
    io.deq <> uart.io.ctl.deq
  }

  class UartLoopbackTests(c: UartLoopback) extends Tester(c, isTrace = false) {
    poke(c.io.deq.valid, 0)

    step(10)

    for (value <- "Hello") {
      while (peek(c.io.enq.ready) == 0) {
        step(1)
      }
      poke(c.io.enq.valid, 1)
      poke(c.io.enq.bits, value.toInt)

      step(1)

      poke(c.io.enq.valid, 0)

      while (peek(c.io.deq.valid) == 0) {
        step(1)
      }
      poke(c.io.deq.ready, 1)
      expect(c.io.deq.bits, value.toInt)

      println("expect: " + value.toInt + ", and got: " + peek(c.io.deq.bits))

      step(1)

      poke(c.io.deq.ready, 0)
    }
  }

  class UartBufferedLoopback extends Module {
    val io = (new UartIO).flip
    val uart = Module(new BufferedUart(0x1ADB, 16))

    uart.io.pins.rxd := uart.io.pins.txd

    io.enq <> uart.io.ctl.enq
    io.deq <> uart.io.ctl.deq
  }

  class UartBufferedLoopbackTests(c: UartBufferedLoopback) extends Tester(c, isTrace = false) {

    poke(c.io.enq.valid, 0)
    poke(c.io.deq.ready, 0)

    step(1)

    def send(values : Seq[Int]) {
      for (value <- values) {
        while (peek(c.io.enq.ready) == 0) {
          step(1)
        }

        poke(c.io.enq.valid, 1)
        poke(c.io.enq.bits, value)

        println("sent: " + value)

        step(1)

        poke(c.io.enq.valid, 0)
      }
    }

    def recv(values : Seq[Int]) {
      poke(c.io.deq.ready, 1)

      for (value <- values) {
        while (peek(c.io.deq.valid) == 0) {
          step(1)
        }

        expect(c.io.deq.bits, value)

        println("recv: " + peek(c.io.deq.bits))

        step(1)
      }

      poke(c.io.deq.ready, 1)

      step(1)
    }

    send(List[Int](0x12, 0x34, 0x56, 0x78, 0x90))
    recv(List[Int](0x12, 0x34, 0x56, 0x78, 0x90))
  }

  class DummyUartTests(c: DummyUart) extends Tester(c, isTrace = false) {
    poke(c.io.enq.valid, 0)
    poke(c.io.deq.ready, 1)
    expect(c.io.enq.ready, 1)

    while (peek(c.io.deq.valid) == 0) {
      step(1)
    }

    while (peek(c.io.deq.valid) == 1) {
      println("recv", peek(c.io.deq.bits))
      step(1)
    }
    poke(c.io.deq.ready, 0)

    val input = Array(0xde, 0xad, 0xbe, 0xef)

    poke(c.io.enq.valid, 1)
    for (b <- input) {
      println("send", b)
      poke(c.io.enq.bits, b)
      step(1)
    }
    poke(c.io.enq.valid, 0)
  }
}
