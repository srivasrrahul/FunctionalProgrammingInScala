import scala.collection.mutable
object Solution {

  def numSubmat(matrix: Array[Array[Int]]): Int = {
    val rowOnes = new mutable.HashMap[Int,Array[Int]]()
    val colOnes = new mutable.HashMap[Int,Array[Int]]()
    for (j <- 0 to matrix(0).length-1) {
      val rowArray = rowOnes.getOrElseUpdate(0,new Array[Int](matrix(0).length))
      val colArray = colOnes.getOrElseUpdate(j,new Array[Int](matrix.length))
      if (j == 0) {
        if (matrix(0)(0) == 1) {
          rowArray(0) = 1
          colArray(0) = 1
        }else {
          rowArray(0) = 0
          colArray(0) = 0
        }
      }else {
        if (matrix(0)(j) == 1) {
          rowArray(j) = rowArray(j-1) + 1
          colArray(0) = 1
        }else {
          rowArray(j) = rowArray(j-1)
          colArray(0) = 0
        }
      }
    }

    for (j <- 0 to matrix.length-1) {
      val colArr = colOnes.getOrElseUpdate(0,new Array[Int](matrix.length))
      val rowArr = rowOnes.getOrElseUpdate(j,new Array[Int](matrix(0).length))
      if (j == 0) {
        if (matrix(0)(0) == 1) {
          colArr(0) = 1
          rowArr(0) = 1
        }else {
          colArr(0) = 0
          rowArr(0) = 0
        }
      }else {
        if (matrix(j)(0) == 1) {
          rowArr(0) = 1
          colArr(j) = colArr(j-1) + 1
        }else {
          rowArr(0) = 0
          colArr(j) = colArr(j-1)
        }
      }
    }

    //    for ((rowId,colArr) <- rowOnes) {
    //      println("rowId " + rowId + " " + colArr.mkString(","))
    //    }
    //
    //    for ((colId,rowArr) <- rowOnes) {
    //      println("colId " + colId + " " + rowArr.mkString(","))
    //    }

    //println("=========")
    for (j <- 1 to matrix.length-1) {
      for (k <- 1 to matrix(0).length-1) {
        if (matrix(j)(k) == 1) {
          val rowArr = rowOnes.getOrElseUpdate(j,new Array[Int](matrix(0).length))
          rowArr(k) = rowArr(k-1) + 1

          val colArr = colOnes.getOrElseUpdate(k,new Array[Int](matrix.length))
          colArr(j) = colArr(j-1) + 1

        }else {
          val rowArr = rowOnes.getOrElseUpdate(j,new Array[Int](matrix(0).length))
          rowArr(k) = rowArr(k-1)

          val colArr = colOnes.getOrElseUpdate(k,new Array[Int](matrix.length))
          colArr(j) = colArr(j-1)

        }
      }
    }

    def allOneCols(colId : Int,row1 : Int,row2 : Int) : Boolean = {
      //      val rows = colOnes.getOrElse(colId,new mutable.TreeSet[Int]()).range(row1,row2+1)
      //      rows.size == (row2-row1+1)
      val colArr = colOnes.get(colId).get
      var sum = colArr(row2)
      if (row1 > 0) {
        sum = sum - colArr(row1-1)
      }

      //println(sum == (row2-row1+1))
      sum == (row2-row1+1)
    }

    def allOneRows(rowId : Int,leftCol : Int,rightCol : Int) : Boolean = {
      //      val cols = rowOnes.getOrElse(rowId,new mutable.TreeSet[Int]()).range(leftCol,rightCol+1)
      //      cols.size == (rightCol-leftCol+1)
      val rowArr = rowOnes.get(rowId).get
      var sum = rowArr(rightCol)
      if (leftCol > 0) {
        sum = sum - rowArr(leftCol-1)
      }

      //println(rowArr.mkString(","))

      //println(sum == (rightCol-leftCol+1))
      sum == (rightCol-leftCol+1)
    }


    //val set = new mutable.HashSet[Rectangle]()
    def topRow(lst : List[Int]) : Int = lst.head
    def bottomRow(lst : List[Int]) : Int = lst.tail.head
    def leftCol(lst : List[Int]) : Int = lst.tail.tail.head
    def rightCol(lst : List[Int]) : Int = lst.tail.tail.tail.head



    val rows = matrix.length
    val cols = matrix(0).length

    //       for ((rowId,colArr) <- rowOnes) {
    //       println("rowId "  + rowId + " " + colArr.mkString(","))
    //     }

    //     for ((colId,rowArr) <- colOnes) {
    //       println("colId "  + colId + " " + rowArr.mkString(","))
    //     }



    val dp = Array.ofDim[Set[List[Int]]](rows,rows,cols,cols)
    for (bRow <- 0 to rows-1) {
      for (eRow <- bRow to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {

            dp(bRow)(eRow)(bCol)(eCol) = Set()
            if (bRow == eRow && bCol == eCol) {
              if (matrix(bRow)(bCol) == 1) {
                dp(bRow)(eRow)(bCol)(eCol) = Set(List(bRow,eRow,bCol,eCol))
              }else {
                dp(bRow)(eRow)(bCol)(eCol) = Set()
              }
            }else {
              if (bRow == eRow) {
                //println("Here " + bCol + " " + eCol)
                if (allOneRows(bRow,bCol,eCol)) {
                  //Total is
                  dp(bRow)(eRow)(bCol)(eCol) = Set(List(bRow,eRow,bCol,eCol))
                }
              }else {
                if (bCol == eCol)  {
                  if (allOneCols(bCol,bRow,eRow)) {
                    dp(bRow)(eRow)(bCol)(eCol) = Set(List(bRow,eRow,bCol,eCol))
                  }
                }
              }
            }
          }
        }
      }
    }


    for (bRow <- rows-1 to 0 by -1) {
      for (eRow <- bRow+1 to rows-1) {
        for (bCol <- cols-1 to 0 by -1) {
          for (eCol <- bCol+1 to cols-1) {
            //println("Here")
            //println(bRow + " " + eRow + " " + bCol + " " + eCol + " ")
            val newSet = new mutable.HashSet[List[Int]]()

            val topRowRemoved = dp(bRow+1)(eRow)(bCol)(eCol)

            //println(topRowRemoved)
            for (s <- topRowRemoved) {
              val sTopRow = s.head
              if (sTopRow == bRow+1 && allOneRows(bRow,leftCol(s),rightCol(s))) {
                //println("Adding top Row" + List(bRow,bottomRow(s),leftCol(s),rightCol(s)))
                newSet.add(List(bRow,bottomRow(s),leftCol(s),rightCol(s)))
              }
            }


            val bottomRowRemoved = dp(bRow)(eRow-1)(bCol)(eCol)

            for (s <- bottomRowRemoved) {
              val sBottomRow = bottomRow(s)
              if (sBottomRow == eRow-1 && allOneRows(eRow,leftCol(s),rightCol(s))) {
                //println("Adding bottom Row" + List(topRow(s),eRow,leftCol(s),rightCol(s)))
                newSet.add(List(topRow(s),eRow,leftCol(s),rightCol(s)))
              }
            }



            val leftColRemoved = dp(bRow)(eRow)(bCol+1)(eCol)

            for (s <- leftColRemoved) {
              val sLeftCol = leftCol(s)
              if (sLeftCol == bCol+1 && allOneCols(bCol,topRow(s),bottomRow(s))) {
                //println("Adding left Col" + List(topRow(s),bottomRow(s),bCol,rightCol(s)))
                newSet.add(List(topRow(s),bottomRow(s),bCol,rightCol(s)))
              }
            }



            val rightColRemoved = dp(bRow)(eRow)(bCol)(eCol-1)
            //println("Right col " + rightColRemoved)
            for (s <- rightColRemoved) {
              val sRightCol = rightCol(s)

              if (sRightCol == eCol-1 && allOneCols(eCol,topRow(s),bottomRow(s))) {
                newSet.add(List(topRow(s),bottomRow(s),leftCol(s),eCol))
              }
            }

            //
            //println("test")
            dp(bRow)(eRow)(bCol)(eCol) = newSet.toSet
            // dp(bRow+1)(eRow)(bCol)(eCol) = Set()
            // dp(bRow)(eRow)(bCol)(eCol-1) = Set()
            // dp(bRow)(eRow-1)(bCol)(eCol) = Set()
            // dp(bRow)(eRow)(bCol+1)(eCol) = Set()
            //println(bRow + " " + eRow + " " + bCol + " " + eCol + " " + newSet.toSet)

          }
        }
      }
    }

    var count = 0
    for (bRow <- 0 to rows-1) {
      for (eRow <- bRow to rows-1) {
        for (bCol <- 0 to cols-1) {
          for (eCol <- bCol to cols-1) {
            count = count + dp(bRow)(eRow)(bCol)(eCol).size
          }
        }
      }
    }

    count





  }
}