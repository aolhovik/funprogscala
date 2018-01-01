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
    *
    * Returns Pascal triangular value for a particular column (c) and row (r)
    **/
  def pascal(c: Int, r: Int): Int = {
    if (c == -1 || c == r + 1) 0 // beyond the triangle
    else if (c == 0 || c == r) 1 // edges
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    val parentheses_opn = '('
    val parentheses_cls = ')'


    def iter(position: Int, bal: Int): Boolean = {

      if (chars.length - 1 < position) // passed all the items
        return bal == 0

      if (bal < 0)
        return false

      val bal_new = bal + {
        if (chars(position) == parentheses_opn)
          1
        else if (chars(position) == parentheses_cls)
          -1
        else 0
      }

      iter(position + 1, bal_new)

    }

    iter(0, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}

