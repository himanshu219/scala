package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }
  test("calculator compute") {
    val references:Map[String, Signal[Expr]] = Map("a"-> Signal(Literal(100)),
                                                   "b"-> Signal(Plus(Literal(2),Literal(100))),
                                                   "c"-> Signal(Times(Ref("a"),Ref("b"))))

    assert(Calculator.computeValues(references).map {
      case (v,s) => (v,s())
    }.toList == List(("a",100.0), ("b",102.0), ("c",10200.0)))
  }

  test("calculator compute NAN") {
    val references:Map[String, Signal[Expr]] = Map("a"-> Signal(Literal(100)),
      "b"-> Signal(Plus(Literal(2),Literal(100))),
      "c"-> Signal(Times(Ref("d"),Ref("b"))))

    assert(Calculator.computeValues(references)("c")().isNaN)
  }
  test("calculator compute cyclic") {
    val references:Map[String, Signal[Expr]] = Map(
      "a"-> Signal(Plus(Ref("b"),Literal(1))),
      "b"-> Signal(Times(Ref("a"),Literal(2))))

//    Calculator.computeValues(references).foreach{
//      case (k,v) => println(k,v())
//    }
    assert(Calculator.computeValues(references)("a")().isNaN)
    assert(Calculator.computeValues(references)("b")().isNaN)
  }
}
