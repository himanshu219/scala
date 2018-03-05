package example

trait Expr {}
case class Sum(leftop: Expr, rgtop: Expr) extends Expr
case class Prod(leftop: Expr, rgtop: Expr) extends Expr
case class Number(n: Int) extends Expr
object exprtree extends App {

  def eval(expr: Expr): Int = expr match {
    case Number(n) => n
    case Sum(leftop, rgtop) => eval(leftop) + eval(rgtop)
    case Prod(leftop, rgtop) => eval(leftop) * eval(rgtop)
  }
  def show(expr: Expr): String = expr match {
    case Number(n) => n.toString()
    case Sum(leftop, rgtop) => show(leftop) + "+" + show(rgtop)
    case Prod(leftop, rgtop) => show(leftop) + "*" +  show(rgtop)
  }
  val n1 = new Number(1)
  val n3 = new Number(3)
  val n4 = new Number(4)
  println(show(new Sum(n1, new Prod(n3, n4))))
}
