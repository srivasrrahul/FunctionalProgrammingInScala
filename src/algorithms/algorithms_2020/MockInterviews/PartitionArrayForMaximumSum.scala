import scala.collection.mutable

case class Index(val j : Int,val k : Int)
object Solution {
  def maxSumAfterPartitioning(arr: Array[Int], K: Int): Int = {
    def getMax(j : Int,k : Int) : Int = {
      var max = arr(j)
      for (p <- j+1 to k) {
        if (arr(p) > max) {
          max = arr(p)
        }
      }

      max
    }
    val cache = new mutable.HashMap[Index,Int]()
    def maxSum(j : Int,k : Int) : Int = {
      if (j > k) {
        0
      }else {
        if (j== k) {
          arr(j)
        }else {
          val index = new Index(j,k)
          if (cache.contains(index) == false) {
            var globalMax = 0
            for (p <- j to j + K - 1 if p <= k) {
              //println(p)
              val leftSum = getMax(j, p) * (p - j + 1)
              val rightSum = maxSum(p + 1, k)

              val total = leftSum + rightSum
              if (total > globalMax) {
                globalMax = total
              }
            }

            cache += ((index,globalMax))
            globalMax
          }else {
            cache.get(index).get
          }
        }
      }
    }

    maxSum(0,arr.length-1)
  }
}