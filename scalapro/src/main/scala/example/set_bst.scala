package example

abstract class Intset {

  def incl(x: Int): Intset

  def contains(x: Int): Boolean

}

class Empty extends Intset {

  def contains(x: Int): Boolean = false

  def incl(x: Int): Intset = new NonEmpty(x, new Empty, new Empty)

  override def toString = "."
}

class NonEmpty(elem: Int, left: Intset, rgt: Intset) extends Intset {

  def incl(x: Int): Intset = {
    if (x < elem)
      new NonEmpty(elem, left incl x, rgt)
    else if (x > elem)
      new NonEmpty(elem, left, rgt incl x)
    else
      this
  }

  def contains(x: Int): Boolean = {
    if (x < elem)
      left contains x
    else if (x > elem)
      rgt contains x
    else
      true
  }

  override def toString = "{" + left +  elem + rgt + "}"
}

object testIntSet extends App{
  val s = new NonEmpty(5, new Empty, new Empty)
//  s = s incl 1
//  s = s incl 7
  val s1 = s incl 1
  val s2 = s1 incl 7
  println(s, s1, s2)
}