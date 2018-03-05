package example


trait Pubilsher {
  private var subscribers: Set[Subscriber] = Set()
  def publish(): Unit = {
    subscribers.foreach(_.handler(this))
  }

  def subscribe(s: Subscriber): Unit = {
    subscribers += s
  }
  def unsubscribe(s: Subscriber): Unit = {
    subscribers -= s
  }
}

trait Subscriber {
  def handler(pub: Pubilsher)
}


class BankAccount() extends Pubilsher {
  private var balance = 0

  def currentBalance = balance

  def deposit(amt: Int): Unit = {
    if (amt > 0) {
      balance += amt
      publish()
    }
  }
  def withDraw(amt: Int): Unit = {
    if (amt > 0 && amt <= balance) {
      balance -= amt
      publish()
    } else throw new Error("Insufficient Funds")
  }
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed.foreach(_.subscribe(this))

  def handler(pub: Pubilsher): Unit = compute()

  def compute(): Unit =
    total = observed.map(_.currentBalance).sum

  private var total: Int = _
  compute()

  def totalBalance = total
}