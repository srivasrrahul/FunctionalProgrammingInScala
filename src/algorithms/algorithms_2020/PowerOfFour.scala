object Solution {
  def isPowerOfFour(num: Int): Boolean = {
    //1
    //100
    //10000
    if (num == 1) {
      true
    }else {
      if ((num & (num - 1)) == 0) {
        //power of two
        val lastDigit = num % 10
        lastDigit == 4 || lastDigit == 6

      } else {
        false
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(isPowerOfFour(256))
  }
}