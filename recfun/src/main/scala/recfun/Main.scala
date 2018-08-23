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
    def pascal(c: Int, r: Int): Int = {
      if ( (c == 0) || (r == 1) )
      {
        /*if (((c == 0) && (r == 0)) ||
          ((c == 0) && (r == 1)) ||
          ((c == 1) && (r == 1)) ||
        )*/
          return 1 // 0,0. 0,1 ; 1,1
        /*
        else
          throw new IllegalArgumentException();*/
      }
      else pascal(c-1, r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
    {
      var open : Int = 0
      var temp = chars
      while ( ! temp.isEmpty )
      {
        if ( temp.head == '(' )
          open = open +1
        else if ( temp.head == ')' )
          open = open -1

        if ( open < 0 )
          return false
        else
          temp = temp.tail

      }

      if ( open == 0 )
        return true
      else
        return false

    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

        if (money < 0 || coins.isEmpty )
          return 0
        if (money == 0)
          return 1
        else
          return countChange( money - coins.head, coins) + countChange(money, coins.tail)

    }
}
