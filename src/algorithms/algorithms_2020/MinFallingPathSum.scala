object Solution {
  def minFallingPathSum(A: Array[Array[Int]]): Int = {
    val matrix = Array.ofDim[Int](A.length,A(0).length)

    for (k <- 0 to A(0).length-1) {
      matrix(A.length-1)(k) = A.last(k)
    }

    for (j <- A.length-2 to 0 by -1) {
      for (k <- 0 to A.length-1) {
        val below = matrix(j+1)(k)
        var left = Int.MaxValue
        if (k-1 >= 0) {
          left = matrix(j+1)(k-1)
        }

        var right = Int.MaxValue
        if (k+1 < A(0).length) {
          right = matrix(j+1)(k+1)
        }

        matrix(j)(k) = A(j)(k) + Array(below,left,right).min
      }
    }

    // println(matrix(0).mkString(","))
    //    println(matrix(1).mkString(","))

    matrix(0).min

  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
    println(minFallingPathSum(arr))
  }
}