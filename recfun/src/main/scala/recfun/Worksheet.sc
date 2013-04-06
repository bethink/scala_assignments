package recfun

object Worksheet {

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


  balance("(if (zero? x) max (/ 1 x))".toList)    //> res0: Boolean = true
  balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
                                                  //> res1: Boolean = true
  balance(":-)".toList)                           //> res2: Boolean = false
	balance("())(".toList)                    //> res3: Boolean = false

}