package example

class SignalBankAccount {
  val balance = Var(0)

  def deposit(amt: Int): Unit = {
    if (amt > 0) {
      val curbal = balance()
      balance.update(amt+ curbal)
    }
  }
  def withDraw(amt: Int): Unit = {
    val curbal = balance()
    if (amt > 0 && amt <=  curbal) {
      balance.update(curbal-amt)
    } else throw new Error("Insufficient Funds")
  }
}
