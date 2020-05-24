object Solution {
  def findLengthOfLCIS(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      //val lcis = new Array[Int](nums.length)
      var maxLcisLen = 1
      var anchorLen = 1
      for (j <- 1 to nums.length - 1) {
        if (nums(j) > nums(j-1)) {
          anchorLen = anchorLen + 1
        }else {
          anchorLen = 1 //start from current
        }

        maxLcisLen = scala.math.max(anchorLen,maxLcisLen)
      }

      maxLcisLen
    }else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    println(findLengthOfLCIS(Array(2,2,2,2)))
  }
}