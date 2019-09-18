import scala.math.max
object Solution {
  def maxSubArray(nums: Array[Int]): Int = {

    var cache = new Array[Int](nums.length)
    cache(0) = nums(0)

    for (j <- 1 to nums.length-1) {
      val currentSum = cache(j-1) + nums(j)
      val currentAloneSum = nums(j)

      cache(j) = scala.math.max(currentSum,currentAloneSum)
    }

    cache.max
//    def itr(index : Int,maxSum : Int) : Int = {
//      index match {
//        case x if x == nums.length => {
//          maxSum
//        }
//        case _ => {
//          var currentMaxSum = nums(index)
//          var currentSum = nums(index)
//          for (j <- index-1 to 0 by -1) {
//            val newSum = currentSum + nums(j)
//            currentSum = newSum
//
//            if (currentSum > currentMaxSum) {
//              currentMaxSum = newSum
//            }
//
//
//          }
//
//          itr(index+1,Math.max(maxSum,scala.math.max(currentMaxSum,maxSum)))
//        }
//      }
//    }
//
//    itr(1,nums(0))
  }

  def main(args: Array[String]): Unit = {
    var arr = Array(-2,1,-3,4,-1,2,1,-5,4)
    println(maxSubArray(arr))
  }
}