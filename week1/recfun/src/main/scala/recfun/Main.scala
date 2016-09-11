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

/*0
0        1
1       1 1
2      1 2 1
3     1 3 3 1
4    1 4 6 4 1
5   1 5 10 10 5 1
6  1 6 15 20 15 6 1

For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3. pascal(1,4)=4 pascal (2,4)=6 ,(3,4) = 4 6,4 = 20
   */

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else {
        val x = r - 1
        pascal(c -1, r - 1) + pascal(c, r - 1)
      }
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = ???
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
