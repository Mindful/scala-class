package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))


    print(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
        if ( c < 0 || r < 0 || c > r) 0
        else if (r <= 1) 1
        else pascal(c, r-1) + pascal(c-1, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def countParenBalance(chars: List[Char], balance: Int ) : Boolean =
          if (balance < 0 ) false
          else if (chars.isEmpty) balance == 0
          else if (chars.head == '(') countParenBalance(chars.tail, balance + 1)
          else if (chars.head == ')') countParenBalance(chars.tail, balance - 1)
          else countParenBalance(chars.tail, balance)


        countParenBalance(chars, 0)

  }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

        def waysToMakeChange(money: Int, coins: List[Int]) : Int =
            if (money == 0) 1
            else if (money > 0 && !coins.isEmpty) waysToMakeChange(money - coins.head, coins) +
              waysToMakeChange(money, coins.tail)
            else 0

        waysToMakeChange(money, coins)
    }

  }
