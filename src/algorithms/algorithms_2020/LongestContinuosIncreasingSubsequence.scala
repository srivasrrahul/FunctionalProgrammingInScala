object Solution {
  def findLengthOfLCIS(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      val lcis = new Array[Int](nums.length)
      lcis(0) = 1
      for (j <- 1 to nums.length - 1) {
        if (nums(j) > nums(j - 1)) {
          lcis(j) = lcis(j - 1) + 1
        } else {
          //start from current
          lcis(j) = 1
        }
      }

      lcis.max
    }else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    println(findLengthOfLCIS(Array(2,2,2,2,2)))
  }
}