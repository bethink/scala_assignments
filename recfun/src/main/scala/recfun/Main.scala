package recfun
import common._

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
   *
   *   r!
   * ---------
   * c! (r-c)!
   *
   */
  def pascal(c: Int, r: Int): Int = {

    def factorial(n: Int): Int =
      if (n <= 1) 1 else n * factorial(n - 1)

    factorial(r) / (factorial(c) * factorial(r - c))
  }

  def balance1(chars: List[Char]): Boolean = {

    var count: Int = 0;
    var isWrong: Boolean = false

    var i: Int = 0
    var size: Int = chars.size
    var char: Char = ' ';

    while (size > i && !isWrong) {

      char = chars(i)

      if (char == '(')
        count = count + 1
      else if (char == ')') {
        if (count <= 0) {
          isWrong = true
        }
        count = count - 1
      }

      i = i + 1
    }

    count == 0
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    var count: Int = 0;

    var checkChar = (char: Int) => {
      if (char == '(')
        1
      else if (char == ')') {
        -1
      } else {
        0
      }
    }

    def checkFirstChar(tail: List[Char]): Int = {

      if (!tail.isEmpty) {

        count = count + checkChar(tail.head)

        if (count >= 0)
          checkFirstChar(tail.tail)
      }

      count
    }

    checkFirstChar(chars)
    
    count == 0
  }                                               //> balance: (chars: List[Char])Boolean


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    1
  }
}
