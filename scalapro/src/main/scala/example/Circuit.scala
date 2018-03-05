package example

abstract class Simulation {

  type Action = () => Unit
  private var curtime = 0
  case class Event(time: Int, action: Action)
  type Agenda = List[Event]
  private var agenda: Agenda = List()

  def currentTime: Int = curtime

  def insert(agenda: Agenda, item: Event): Agenda = agenda match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: agenda
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    var item = Event(curtime+delay, () => block)
    agenda = insert(agenda, item)
  }

  def loop(): Unit = agenda match {
    case first :: rest =>
      curtime = first.time
      agenda = rest
      first.action()
      loop()
    case Nil =>
  }
  def run(): Unit = {
    afterDelay(0) {
      println("Simulation started time: " + currentTime)
    }
    loop()
  }

}

trait Parameters {
  def Inverterdelay = 2
  def AndGatedelay = 3
  def OrGatedelay = 5

}
abstract class Gates extends Simulation {

  def Inverterdelay : Int
  def AndGatedelay : Int
  def OrGatedelay : Int

  class Wire {
    private var sig = false
    private var actions: List[Action] = List()
    def getSignal: Boolean = sig
    def setSignal(s: Boolean): Unit =
      if (s != sig) {
        sig = s
        actions.foreach(_())
      }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(inp: Wire, out: Wire): Unit = {
    def invertAction(): Unit = {
      var a = inp.getSignal
      afterDelay(Inverterdelay) {
        out.setSignal(!a)
        println("calling not gate", out.getSignal)
      }
    }
    inp.addAction(invertAction)
  }

  def andGate(inp1: Wire, inp2: Wire, out: Wire): Unit = {
    def andAction(): Unit = {
      var a = inp1.getSignal
      var b = inp2.getSignal
      afterDelay(AndGatedelay) {
        out.setSignal(a&b)
        println("calling and gate", out.getSignal)
      }
    }
    inp1.addAction(andAction)
    inp2.addAction(andAction)
  }

  def orGate(inp1: Wire, inp2: Wire, out: Wire): Unit = {
    def orAction(): Unit = {
      var a = inp1.getSignal
      var b = inp2.getSignal
      afterDelay(OrGatedelay) {
        out.setSignal(a|b)
        println("calling or gate", out.getSignal)
      }
    }
    inp1.addAction(orAction)
    inp2.addAction(orAction)
  }
  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime ${wire.getSignal}")
    }

    wire.addAction(probeAction)
  }

}

abstract class Circuits extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    //c = a&b
    //s = (a|b)|(!a&b)
    var d = new Wire
    var e = new Wire
    orGate(a,b,d)
    andGate(a,b,c)
    inverter(c,e)
    orGate(d,e,s)
  }
  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    var s,c1,c2 = new Wire
    halfAdder(a,cin,s,c1)
    halfAdder(b,s, sum, c2)
    orGate(c1, c2, cout)
  }

}
