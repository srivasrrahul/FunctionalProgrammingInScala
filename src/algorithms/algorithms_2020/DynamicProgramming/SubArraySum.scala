object Solution {
  def maxSubArray(nums: Array[Int]): Int = {
    val maxSum = new Array[Int](nums.length)
    if (nums.isEmpty) {
      0
    }else {
      maxSum(0) = nums(0)
      for (j <- 1 to nums.length-1) {
        maxSum(j) = scala.math.max(nums(j),nums(j) + maxSum(j-1))
      }

      println(maxSum.mkString(","))
      maxSum.max
    }

  }

  def main(args: Array[String]): Unit = {
    println(maxSubArray(Array(-1,2,3)))
  }
}