package example

trait mList[T] {
  def isEmpty: Boolean
  def head: T
  def tail: mList[T]
}

class Cons[T](val head: T, val tail: mList[T]) extends mList[T] {

  def isEmpty = false


}

class Nil[T] extends mList[T] {
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}
