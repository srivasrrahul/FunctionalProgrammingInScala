object Solution {
  def longestSubarray(nums: Array[Int], limit: Int): Int = {

    var lowerLimit = 0
    var maxArrLen = 1
    for (j <- 1 to nums.length-1) {
      var maxAbsDiff = 1
      var shudGoFurther = true
      for (k <- j-1 to lowerLimit by -1 if shudGoFurther == true) {

        val diff = math.abs(nums(j) - nums(k))
        if (diff <= limit) {
          maxAbsDiff = maxAbsDiff + 1
        } else {
          shudGoFurther = false
          lowerLimit = k + 1
        }

      }

      if (maxAbsDiff > maxArrLen)  {
        maxArrLen = maxAbsDiff
      }

      //println("for j " + j + " " + lowerLimit + " " + maxAbsDiff)
    }

    maxArrLen
  }
}