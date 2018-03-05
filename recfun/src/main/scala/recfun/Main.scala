package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(r: Int, n: Int): Int =  {
      if (r == 0 || r == n) 1 else pascal(r-1, n-1) + pascal(r, n-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isbal(ch: List[Char], cnt: Int): Boolean = {
        if (ch.isEmpty) {
          cnt == 0
        } else {
          if (cnt < 0) {
            false
          }
          else {
            if (ch.head == '(') {
              isbal(ch.tail, cnt + 1)
            } else if (ch.head == ')') {
              isbal(ch.tail, cnt - 1)
            } else {
              isbal(ch.tail, cnt)
            }
          }
        }
      }
      isbal(chars, 0)
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (money < 0 || coins.isEmpty) {
        0
      } else {
        countChange(money, coins.tail) + countChange(money-coins.head, coins)
      }
    }
  }
