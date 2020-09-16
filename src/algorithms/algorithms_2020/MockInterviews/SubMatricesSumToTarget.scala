import scala.collection.mutable

object Solution {
  def numSubmatrixSumTarget(matrix: Array[Array[Int]], target: Int): Int = {
    val rows = matrix.length
    val cols = matrix(0).length
    //val sums = Array.ofDim[Int](rows,rows,cols,cols)


    //Each col of size 1 and starting sum
    var count = 0

    val prefixSums = new mutable.HashMap[Int,Array[Int]]()
    for (j <- 0 to rows-1) {
      val prefixSum = new Array[Int](cols)
      prefixSum(0) = matrix(j)(0)
      for (k <- 1 to cols-1) {
        prefixSum(k) = prefixSum(k-1) + matrix(j)(k)
      }

      prefixSums += ((j,prefixSum))
    }

    def getColSum(k : Int,p : Int,q : Int) : Int = {
      val prefixSum = prefixSums.get(k).get
      var total = prefixSum(q)
      if (p > 0) {
        total = total - prefixSum(p-1)
      }

      total
    }


    for (j <- 0 to rows-1) {
      val arr = Array.ofDim[Int](cols,cols)

      for (k <- j to rows-1) {
        val newArr = Array.ofDim[Int](cols,cols)

        for (p <- 0 to cols-1) {
          for (q <- p to cols-1) {

            if (j == k) {
              val sum = getColSum(j,p,q)
              newArr(p)(q) = sum
              if (target == sum) {
                count = count + 1
              }
            }else {
              val newSum = arr(p)(q) + getColSum(k,p,q)
              newArr(p)(q) = newSum
              if (target == newSum) {
                count = count+1
              }
            }
          }
        }

        for (x <- 0 to cols-1) {
          for (y <- 0 to cols-1) {
            arr(x)(y) = newArr(x)(y)
          }
        }
      }
    }

    count
  }
}