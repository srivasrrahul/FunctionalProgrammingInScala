import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ColIndex(val bCol : Int,val eCol : Int)
object Solution {
  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    if (matrix.length == 0) {
      0
    }else {
      val rows = matrix.length
      val cols = matrix(0).length
      val dp = Array.ofDim[Set[Int]](rows, rows) //List of eCols


      var maxArea = 0
      for (bRow <- 0 to rows - 1) {
        val lstBuffer = new mutable.HashSet[Int]
        for (bCol <- 0 to cols - 1) {
          if (matrix(bRow)(bCol) == '1') {
            lstBuffer.add(bCol)
            maxArea = 1 //begins at 1 and ends at 1
          } else {
            //dp(bRow)(bRow)(bCol)(bCol) = false
          }

        }

        dp(bRow)(bRow) = lstBuffer.toSet //List of eCol and of size 1
      }


      val rowOnes = new mutable.HashMap[Int, Array[Int]]()
      val colOnes = new mutable.HashMap[Int, Array[Int]]()

      for (j <- 0 to rows - 1) {
        for (k <- 0 to cols - 1) {
          val rowOne = rowOnes.getOrElseUpdate(j, new Array[Int](cols))
          val colOne = colOnes.getOrElseUpdate(k, new Array[Int](rows))
          if (matrix(j)(k) == '1') {
            if (k == 0) {
              rowOne(k) = 1
            } else {
              rowOne(k) = rowOne(k - 1) + 1
            }

            if (j == 0) {
              colOne(j) = 1
            } else {
              colOne(j) = colOne(j - 1) + 1
            }

          } else {
            if (k == 0) {
              rowOne(k) = 0
            } else {
              rowOne(k) = rowOne(k - 1)
            }

            if (j == 0) {
              colOne(j) = 0
            } else {
              colOne(j) = colOne(j - 1)
            }
          }
        }
      }

      // for ((id,arr) <- rowOnes) println(id + " " + arr.mkString(","))
      // println("=============================")
      // for ((id,arr) <- colOnes) println(id + " " + arr.mkString(","))
      //println(colOnes)
      // for (t <- 0 to dp.length-1) {
      //   // for (u <- 0 to dp(0).length-1) {
      //   //     print(dp(t)(u)+ ",")
      //   // }
      //   println(dp(t).mkString(","))
      //   //println()
      // }
      def allRowOnes(rowId: Int, bCol: Int, eCol: Int): Boolean = {
        val rowOne = rowOnes.get(rowId).get
        var total = rowOne(eCol)
        if (bCol > 0) {
          total = total - rowOne(bCol - 1)
        }

        total == (eCol - bCol + 1)
      }

      def allColOnes(colId: Int, bRow: Int, eRow: Int): Boolean = {
        val colOne = colOnes.get(colId).get
        var total = colOne(eRow)
        if (bRow > 0) {
          total = total - colOne(bRow - 1)
        }

        total == (eRow - bRow + 1)
      }

      for (bRow <- 0 to rows - 1) {

        for (eRow <- bRow + 1 to rows - 1) {
          //1*1,2*2,3*3
          val rowCount = eRow - bRow + 1
          val set = new mutable.HashSet[Int]()
          for (bCol <- 0 to cols-1) {
            val eCol = bCol + (rowCount-1)
            if (eCol > 0 && eCol < cols) {
              if (dp(bRow)(eRow-1).contains(eCol-1) && allRowOnes(eRow,bCol,eCol) && allColOnes(eCol,bRow,eRow)) {
                //println("Found " + bRow + " " + eRow + " " + bCol + " " + eCol)
                set.add(eCol)
                val area = (eCol-bCol+1)*(eCol-bCol+1)
                if (area > maxArea) {
                  maxArea = area
                }
              }
            }
          }

          dp(bRow)(eRow) = set.toSet
        }
      }

      //Traverse through all row
      //var maxArea = 0
      //      for (bRow <- 0 to rows - 1) {
      //        for (eRow <- bRow to rows - 1) {
      //          for ((colBegin,colEnd) <- dp(bRow)(eRow)) {
      //            val area = (colEnd-colBegin+1)*(colEnd-colBegin+1)
      //            if (area > maxArea) {
      //              maxArea = area
      //            }
      //          }
      //        }
      //      }

      maxArea
    }
  }
}