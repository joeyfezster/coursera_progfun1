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
        if (c == 0 || c == r) 1
        else pascal(c - 1, r - 1) + pascal(c, r - 1)
        //      def factorial(n: Int): Int = {
        //        def loop(acc: Int, n: Int): Int =
        //          if (n==1) acc
        //          else loop(acc*n, n-1)
        //
        //        if(n == 0) 1
        //        else loop(1, n)
        //      }
        //
        //      def choose(n: Int, t: Int): Int = {
        //        factorial(n)/(factorial(t)*factorial(n-t))
        //      }
        //      choose(c,r)
    }

    /**
      * Exercise 2
      */
    def balance(chars: List[Char]): Boolean = {
        def loop(acc: Int, chars: List[Char]): Boolean = {
            if (acc < 0) false
            else if (chars.isEmpty && acc != 0) false
            else if (chars.isEmpty && acc == 0) true
            else loop(account(acc, chars.head), chars.tail)
        }

        def account(acc: Int, c: Char): Int = {
            if (c == '(') acc + 1
            else if (c == ')') acc - 1
            else acc
        }

        loop(0, chars)
    }

    /**
      * Exercise 3
      */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money < 0 || coins.isEmpty) 0
        else if (money == 0) 1
        else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}
