object Solution {
  def maxSumTwoNoOverlap(A: Array[Int], L: Int, M: Int): Int = {
    val arrSum = new Array[Int](A.length)
    arrSum(0) = A(0)
    for (j <- 1 to A.length-1) {
      arrSum(j) = arrSum(j-1) + A(j)
    }

    //println(arrSum.mkString(","))
    def getSum(index : Int,size : Int) : Int = {
      if (index + size-1 < A.length) {
        val finalIndex = index + size-1
        var finalSum = arrSum(finalIndex)
        if (index > 0) {
          finalSum - arrSum(index-1)
        }else {
          finalSum
        }
      }else {
        -1
      }
    }

    var maxSum = Int.MinValue
    for (j <- 0 to A.length-1) {
      val lSum = getSum(j,L)
      for (k <- j+L to A.length-1) {
        if (k+M-1 < A.length) {
          val mSum = getSum(k,M)
          if (lSum + mSum > maxSum) {
            maxSum = lSum + mSum
          }
        }
      }
    }

    for (j <- 0 to A.length-1) {
      val mSum = getSum(j,M)
      for (k <- j+M to A.length-1) {
        if (k+L-1 < A.length) {
          val lSum = getSum(k,L)
          if (lSum + mSum > maxSum) {
            maxSum = lSum + mSum
          }
        }
      }
    }

    maxSum
  }

  def main(args: Array[String]): Unit = {
    println(maxSumTwoNoOverlap(Array(0,6,5,2,2,5,1,9,4),1,2))
  }
}