import scala.collection.mutable

object Solution {
  def maxSumSubmatrix(matrix: Array[Array[Int]], K: Int): Int = {
    val rows = matrix.length
    val cols = matrix(0).length


    val rowSums = new mutable.HashMap[Int,Array[Int]]()

    for (j <- 0 to rows-1) {
      val rowSum = rowSums.getOrElseUpdate(j,new Array[Int](cols))
      rowSum(0) = matrix(j)(0)

      for (k <- 1 to cols-1) {
        rowSum(k) = rowSum(k-1) + matrix(j)(k)
      }
    }

    def getRowSum(j : Int,bCol : Int,eCol : Int) : Int = {
      val rowSum = rowSums.getOrElseUpdate(j,new Array[Int](cols))
      var total = rowSum(eCol)
      if (bCol > 0) {
        total = total - rowSum(bCol-1)
      }

      total
    }

    //val sums = Array.ofDim[Int](rows,rows)
    val sums = Array.ofDim[Int](cols,cols)

    //println(rowSums.get(0).get.mkString(","))
    var globalMax = Int.MinValue
    //    for (j <- 0 to rows-1) {
    //      for (k <- 0 to cols-1) {
    //        for (p <- k to cols-1) {
    //          sums(j)(j)(k)(p) = getRowSum(j,k,p)
    //          val s = sums(j)(j)(k)(p)
    //          //println(k + " " + p + " : " + s)
    //          if (s <= K && s > globalMax) {
    //            globalMax = s
    //            //println("Global max " + globalMax)
    //          }
    //        }
    //      }
    //    }


    //println(rowSums.get(0).get.mkString(","))
    //println(rowSums.get(1).get.mkString(","))

    for (bRow <- 0 to rows-1) {
      for (eRow <- bRow to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {
            //println("Here")
            if (bRow == eRow) {
              sums(bCol)(eCol) = getRowSum(eRow,bCol,eCol)
            }else {
              sums(bCol)(eCol) = sums(bCol)(eCol) + getRowSum(eRow,bCol,eCol)
            }

            val s = sums(bCol)(eCol)
            //println(bRow + " " + eRow + " " + bCol + " " + eCol + " => " + s + " ctxt " + sums(bRow)(eRow-1)(bCol)(eCol))
            if (s <= K && s > globalMax) {
              globalMax = s
            }
          }
        }
      }
    }

    globalMax



  }
}