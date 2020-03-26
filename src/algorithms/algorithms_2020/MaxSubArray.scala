import util.control.Breaks._

object Solution {
  def maxSubArray(nums: Array[Int]): Int = {
    val maxSumArray = new Array[Int](nums.size)
    maxSumArray(0) = nums(0)

    for (j <- 1 to nums.length-1) {

      val maxSumForCurrent = nums(j)
      val maxEarly = nums(j) + maxSumArray(j-1)
      maxSumArray(j) = scala.math.max(maxSumForCurrent,maxEarly)

    }

    maxSumArray.max
  }

  def main(args: Array[String]): Unit = {
    println(maxSubArray(Array(-2,1,-3,4,-1,2,1,-5,4)))
  }
}