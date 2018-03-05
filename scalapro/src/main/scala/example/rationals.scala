package example

class Rational(n: Int, d: Int) {

  require(d != 0,"Denominator must be non zero")

  def this(x: Int) = this(x, 1)

//  why not this(x: Int): Rational = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer: Int = n / gcd(n, d)

  val denom: Int = d / gcd(n, d)

  def add(that: Rational): Rational = new Rational(numer*that.denom + that.numer*denom, denom*that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational): Rational = add(that.neg)

  def less(that: Rational): Boolean = numer*that.denom < that.numer*denom

  def max(that: Rational): Rational = if (less(that)) that else this

  override def toString: String = numer + "/" + denom

}


object example extends App{

  val a = new Rational(1,3)
  val b = new Rational(3,6)
  println (a.add(b))
  println (a.less(b))
  println (a.max(b))


}