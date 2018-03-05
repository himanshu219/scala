package example

object week5 extends App{

//  def msort[T](arr: List[T])(lt: (T, T) => Boolean): List[T] = {
//    val pivot = arr.length / 2
//    if (pivot == 0) arr
//    else {
//      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
//        case (Nil, ys) => ys
//        case (xs, Nil) => xs
//        case (x :: xs1, y :: ys1) =>
//          if (lt(x,y)) x :: merge(xs1, ys)
//          else y :: merge(xs, ys1)
//
//      }
//      val (la, ra) = arr splitAt pivot
//      merge(msort(la)(lt), msort(ra)(lt))
//    }
//  }
  def msort[T](arr: List[T])(implicit ord: Ordering[T]): List[T] = {
    val pivot = arr.length / 2
    if (pivot == 0) arr
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x,y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)

      }
      val (la, ra) = arr splitAt pivot
      merge(msort(la), msort(ra))
    }
  }
  println(msort(List(4,5,1,2,0,78,2,3)))

}
