import example.example

//abstract class Sqclass (a: Int) {
//    require(a > 0, "require positive number")
//    def square(): Int = {
//      a*a
//    }
//    def cube(a: Int): Int
//
////    private def cube2(b: Int): Int = _
//
//    override def toString = {
//      " Num, " + a
//    }
//
//}
////val obj = new Sqclass(2)
////obj square
//
//abstract class TopLevel {     // abstract class
//  def method1(x: Int): Int   // abstract method
//def method2(x: Int): Int = {4}
//}
//
//class Level1 extends TopLevel {
//  def method1(x: Int): Int = {2}
//  override def method2(x: Int): Int = {3}
//  // TopLevel's method2 needs to be explicitly overridden
//}
//
//val list = List("Apple", "Orange", "Pine")
//val alist = "Apple" :: "Orange" :: "Pine" :: Nil
//
//alist
//list
//list == alist
//list.tail
////array vs vector(it's a tree so access is logn)
////stream vs list
//// range vs seq vs Ordering
//(1 to 6 by 2).toSet
//val s = "Himanshu"
//s.charAt(2)
//
//s.filter(x => x.isUpper)
//val prime = List(1,2,3,4,5)
//prime take 3
//prime drop 2
//prime take -3
//prime drop -2
//prime.reduceLeft((x,y)=>x+y)
//prime.foldLeft(10)((x,y)=>x+y)
//val a, b = prime partition (x=>x>3)
//
//List(1,4,16).reduceRight((x,y)=>y/x)
//
//List().init
//
def isGood(num: Double, guess: Double): Boolean = {
  if (math.abs(guess*guess - num)/ num < 0.001) true else false
}
def improve(num: Double, guess: Double): Double = {
  (guess + num/guess)/2
}
def sqrt(num: Double, guess: Double): Double = {
  if (isGood(num, guess)) {
    guess
  } else {
    val newguess = improve(num, guess)
    sqrt(num, newguess)
  }

}
sqrt(2,1)
sqrt(16,1)
sqrt(1.0e50,1)

def fact(n: Long, res: Long): Long = {
  if (n == 0) res else fact(n-1, n*res)
}
fact(5,1)


def from(n: Int): Stream[Int] = n #:: from(n+1)

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
}
//val primes = sieve(from(1))

package example
val prob = Pouring(Vector(4,7))
println(prob.moves)


