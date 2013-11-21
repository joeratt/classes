package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " + wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output.setSignal(a1Sig & a2Sig)
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1Sig | a2Sig)
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val wireA1 = new Wire
    val wireA2 = new Wire
    val invertedOutput = new Wire
    inverter(a1, wireA1)
    inverter(a2, wireA2)
    andGate(wireA1, wireA2, invertedOutput)
    inverter(invertedOutput, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    val numOut = out.length
    val numC = c.length
    //    def demuxAction() {
    /*if (numC == 0) andGate(in, in, out(0))
    else if(numC == 1) {
      val invC = new Wire()
      inverter(c(0),invC)
      andGate(in,c(0),out(0))
      andGate(in,invC,out(1))
    }
    else
      Nil*/
    //    }
    def demuxHelper(in: Wire, currIdx: Int, currOutIdx: Int) {
      println(s"currIdx: $currIdx, currOutId$currOutIdx")
      if (numC == 0) andGate(in, in, out(0))
      else if (currIdx >= numC) Nil
      else {
        //for (i <- 0 until currIdx) {
        //val currOutIdx = 2*i
        val invC = new Wire()
        //inverter(c(currIdx), invC)
        invC setSignal !c(currIdx).getSignal
        out(currOutIdx).setSignal(in.getSignal & invC.getSignal)
        out(currOutIdx - 1).setSignal(in.getSignal & c(currIdx).getSignal)
        //        andGate(in, invC, out(currOutIdx))
        //        andGate(in, c(currIdx), out(currOutIdx - 1))
        demuxHelper(out(currOutIdx), currIdx + 1, currOutIdx)
        demuxHelper(out(currOutIdx - 1), currIdx + 1, currOutIdx - 2)
        //}
      }
    }

    def demuxAction() {
      demuxHelper(in, 0, numOut - 1)
    }

    in addAction demuxAction
    for (curC <- c) curC addAction demuxAction
  }


}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
