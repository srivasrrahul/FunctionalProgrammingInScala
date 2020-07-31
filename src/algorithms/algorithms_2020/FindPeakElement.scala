object Solution {
  def findPeakElement(nums: Array[Int]): Int = {
    var prev = nums

    var found = -1
    for (j <- 0 to nums.length-1 if found == -1) {
      var test = true
      if (j > 0) {
        if (nums(j) < nums(j-1)) {
          test = false
        }
      }

      if (j < nums.length-1) {
        if (nums(j) < nums(j+1)) {
          test = false
        }
      }

      if (test) {
        found = j
      }
    }

    found
  }
}