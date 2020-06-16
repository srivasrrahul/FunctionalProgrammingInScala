object Solution {
  def mySqrt(x: Int): Double = {
    def good_enough(guess : Double) : Boolean = {
      val diff = scala.math.abs(guess*guess - x)
      diff <= 0.01
    }
    def improve_guess(guess : Double) : Double = {
      val new_guess = (guess + (x/guess))/2.0
      new_guess
    }
    def itr(guess : Double) : Double = {
      //println(guess)
      if (good_enough(guess)) {
        guess
      }else {
        itr(improve_guess(guess))
      }
    }

    itr(1.0)
  }

  def isPerfectSquare(num: Int): Boolean = {
    val sqrt = mySqrt(num)
    val squareVal = sqrt*sqrt
    //println(squareVal)
    scala.math.abs(sqrt-sqrt.toInt) < 0.01

  }

  def main(args: Array[String]): Unit = {
    println(isPerfectSquare(4))
  }
}