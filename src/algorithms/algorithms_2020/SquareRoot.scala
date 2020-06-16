object Solution {
  def mySqrt(x: Int): Int = {
    def good_enough(guess : Double) : Boolean = {
      val diff = scala.math.abs(guess*guess - x)
      diff <= 0.01
    }
    def improve_guess(guess : Double) : Double = {
      val new_guess = (guess + (x/guess))/2.0
      new_guess
    }
    def itr(guess : Double) : Double = {
      println(guess)
      if (good_enough(guess)) {
        guess
      }else {
        itr(improve_guess(guess))
      }
    }

    itr(1.0).toInt
  }

  def main(args: Array[String]): Unit = {
    println(mySqrt(124))
  }
}