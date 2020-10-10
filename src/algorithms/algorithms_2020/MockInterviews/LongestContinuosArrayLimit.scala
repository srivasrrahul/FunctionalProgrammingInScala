object Solution {
  def longestSubarray(nums: Array[Int], limit: Int): Int = {
    val countArrLen = new Array[Int](nums.length)
    if (0 <= limit) {
      countArrLen(0) = 1
    }

    for (j <- 1 to nums.length-1) {
      var maxAbsDiff = 1
      var shudGoFurther = true
      var lowerLimit = 0
      for (k <- j-1 to 0 by -1 if shudGoFurther == true && k >= lowerLimit) {
        val diff = math.abs(nums(j)-nums(k))
        if (diff <= limit) {
          maxAbsDiff = maxAbsDiff + 1
        }else {
          shudGoFurther = false
        }

        lowerLimit = k-countArrLen(k) + 1
      }

      countArrLen(j) = maxAbsDiff
    }

    countArrLen.max
  }
}