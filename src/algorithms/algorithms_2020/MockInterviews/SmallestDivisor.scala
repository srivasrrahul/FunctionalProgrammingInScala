object Solution {
  def smallestDivisor(nums: Array[Int], threshold: Int): Int = {
    def checkValue(div : Int) : Boolean = {
      var sum = 0
      for (num <- nums) {
        sum = sum + math.ceil(num.toDouble/div).toInt
      }

      sum <= threshold
    }

    def itr(low : Int,high : Int) : Int = {
      if (high == low) {
        low
      }else {
        val mid = low + (high-low)/2
        if (checkValue(mid)) {
          itr(low,mid)
        }else {
          itr(mid+1,high)
        }
      }
    }

    itr(1,math.abs(nums.max))
  }
}