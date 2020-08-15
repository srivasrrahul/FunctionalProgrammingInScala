object Solution {
  def isPowerOfFour(num: Int): Boolean = {
    if (num <= 0) {
      false
    }else {
      def isPowerTwo(x : Int) : Boolean = {
        (x & (x-1)) == 0
      }

      if (isPowerTwo(num)) {
        //if power is an even
        val l = (math.log(num)/math.log(2)).toInt
        l % 2 == 0

      }else {
        false
      }
    }

  }
}