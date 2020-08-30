import scala.collection.mutable
object Solution {

  def numSubmat(matrix: Array[Array[Int]]): Int = {
    val rowOnes = new mutable.HashMap[Int,Array[Int]]()
    //val colOnes = new mutable.HashMap[Int,mutable.TreeSet[Int]]()

    for (j <- 0 to matrix.length-1) {
      for (k <- 0 to matrix(0).length-1) {
        if (matrix(j)(k) == 1) {
          val cols = rowOnes.getOrElseUpdate(j,new Array[Int](matrix(0).length))
          if (k > 0) {
            cols(k) = cols(k-1) + 1
          }else {
            cols(k) = 1
          }

          // val rows = colOnes.getOrElseUpdate(k,new mutable.TreeSet[Int]())
          // rows.add(j)
        }else {
          val cols = rowOnes.getOrElseUpdate(j,new Array[Int](matrix(0).length))
          if (k > 0) {
            cols(k) = cols(k-1)
          }else {
            cols(k) = 0
          }
        }
      }
    }

    def allOneRows(rowId : Int,leftCol : Int,rightCol : Int) : Boolean = {
      val cols = rowOnes.get(rowId).get
      var sum = cols(rightCol)
      if (leftCol > 0) {
        sum = sum - cols(leftCol-1)
      }

      sum == (rightCol-leftCol+1)
      //      val rowArr = rowOnes.get(rowId).get
      //      var sum = rowArr(rightCol)
      //      if (leftCol > 0) {
      //        sum = sum - rowArr(leftCol-1)
      //      }
      //
      //      //println(rowArr.mkString(","))
      //
      //      //println(sum == (rightCol-leftCol+1))
      //      sum == (rightCol-leftCol+1)
    }


    val rows = matrix.length
    val cols = matrix(0).length


    val dp = Array.ofDim[Boolean](rows,rows,cols,cols)
    //println("Hello " + rowOnes.size + " " + colOnes.size)
    for (bRow <- 0 to rows-1) {
      for (eRow <- bRow to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {

            dp(bRow)(eRow)(bCol)(eCol) = false
            if (bRow == eRow && bCol == eCol) {
              if (matrix(bRow)(bCol) == 1) {
                dp(bRow)(eRow)(bCol)(eCol) = true
              }
            }else {
              if (bRow == eRow) {
                //println("Here " + bCol + " " + eCol)
                if (allOneRows(bRow,bCol,eCol)) {
                  //Total is
                  dp(bRow)(eRow)(bCol)(eCol) = true
                }
              }else {
                // if (bCol == eCol)  {
                //   if (allOneCols(bCol,bRow,eRow)) {
                //     dp(bRow)(eRow)(bCol)(eCol) = 1
                //   }
                // }
              }
            }
          }
        }
      }
    }



    for (bRow <- rows-1 to 0 by -1) {
      for (eRow <- bRow+1 to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {

            var newCount = 0

            val topRowRemovedCount = dp(bRow+1)(eRow)(bCol)(eCol)
            if (topRowRemovedCount == true && allOneRows(bRow,bCol,eCol)) {
              dp(bRow)(eRow)(bCol)(eCol) = true
            }



          }
        }
      }
    }

    var count = 0
    for (bRow <- 0 to rows-1) {
      for (eRow <- bRow to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {
            if (dp(bRow)(eRow)(bCol)(eCol) == true) {
              count = count + 1
            }

          }
        }
      }
    }

    count





  }
}