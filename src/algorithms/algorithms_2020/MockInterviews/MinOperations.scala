
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def minOperations(nums: Array[Int], x: Int): Int = {
    val sums = new Array[Int](nums.length)
    sums(0) = nums(0)

    for (j <- 1 to nums.length-1) {
      sums(j) = sums(j-1) + nums(j)
    }

    var sums1 = new Array[Int](nums.length)
    sums1(sums1.length-1) = nums.last


    for (j <- sums1.length-2 to 0 by -1) {
      sums1(j) = sums1(j+1) + nums(j)
    }

    sums1 = sums1.reverse

    def getSum(j : Int,k : Int) : Int = {
      if (j > k) {
        0
      }else {
        var s = sums(k)
        if (j > 0) {
          s = s - sums(j - 1)
        }

        s
      }
    }

    def getSum1(j : Int, k : Int) : Int = {
      if (j > k) {
        0
      }else {
        var s = sums1(k)
        if (j > 0) {
          s = s - sums(j - 1)
        }

        s
      }
    }

    if (sums.last < x || sums.head > x) {
      -1
    }else {
      def getNetSum(left : Int,right : Int) : Int = {
        val lSum = getSum(0,left-1)
        val rSum = getSum(sums.length-right,sums.length-1)

        lSum + rSum
      }

      val lMax = sums.search(x)
      var lCount = lMax
      var rCount = 0

      var minSize = Int.MaxValue
      //println(sums.mkString(","))
      //println(sums1.mkString(","))
      for (j <- 0 to nums.length) {
        val leftSize = j
        val leftSum = getSum(0,leftSize-1)
        //println(leftSum)
        if (leftSum == x) {
          minSize = math.min(minSize,leftSize)
        }else {
          if (x > leftSum) {
            val pending = x-leftSum
            // println("Pending " + pending)
            sums1.search(pending,0,nums.length-j) match {
              case scala.collection.Searching.Found(foundIndex) => {
                //println("foundIndex " + foundIndex + " " + pending)
                minSize = math.min(minSize,leftSize+(foundIndex+1))
              }
              case _ => {

              }
            }
          }
        }
      }

      minSize
    }


  }

  def main(args: Array[String]): Unit = {
    println(minOperations(Array(5,6,7,8,9),4))
  }
}