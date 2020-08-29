import scala.collection.mutable

case class Point(val x : Int,val y : Int) {
  def down() : Point = {
    new Point(x+1,y)
  }

  def up() : Point = {
    new Point(x-1,y)
  }

  def left() : Point = {
    new Point(x,y-1)
  }

  def right() : Point = {
    new Point(x,y+1)
  }
}
case class Rectangle(val topLeft : Point,val topRight : Point,val bottomLeft : Point,val bottomRight : Point) {
  def rowSize() : Int = {
    bottomLeft.x-topLeft.x+1
  }
  def colSize() : Int = {
    topRight.y-topLeft.y+1
  }
  def isSizeOne() : Boolean = {
    colSize() == 1 && rowSize() == 1
  }

  def removeTopRow() : Rectangle = {
    new Rectangle(topLeft.down(),topRight.down(),bottomLeft,bottomRight)
  }



  def removeBottomRow() : Rectangle = {
    new Rectangle(topLeft,topRight,bottomLeft.up(),bottomRight.up())
  }



  def removeLeftCol() : Rectangle = {
    new Rectangle(topLeft.right(),topRight,bottomLeft.right(),bottomRight)
  }



  def removeRightCol() : Rectangle = {
    new Rectangle(topLeft,topRight.left(),bottomLeft,bottomRight.left())
  }




}
object Solution {

  type RowID=Int
  type ColId=Int
  def numSubmat(matrix: Array[Array[Int]]): Int = {
    val rowOnes = new mutable.HashMap[RowID,mutable.TreeSet[Int]]()
    val colOnes = new mutable.HashMap[ColId,mutable.TreeSet[Int]]()
    for (j <- 0 to matrix.length-1) {
      for (k <- 0 to matrix(0).length-1) {
        if (matrix(j)(k) == 1) {
          val colSet = rowOnes.getOrElseUpdate(j,new mutable.TreeSet[Int]())
          colSet.add(k)

          val rowSet = colOnes.getOrElseUpdate(k,new mutable.TreeSet[Int]())
          rowSet.add(j)
        }
      }
    }

    def allOneCols(colId : Int,row1 : Int,row2 : Int) : Boolean = {
      val rows = colOnes.getOrElse(colId,new mutable.TreeSet[Int]()).range(row1,row2+1)
      rows.size == (row2-row1+1)
    }

    def allOneRows(rowId : Int,leftCol : Int,rightCol : Int) : Boolean = {
      val cols = rowOnes.getOrElse(rowId,new mutable.TreeSet[Int]()).range(leftCol,rightCol+1)
      cols.size == (rightCol-leftCol+1)
    }



    def getValue(rectangle: Rectangle) : Int = {
      matrix(rectangle.topLeft.x)(rectangle.topLeft.y)
    }



    //val set = new mutable.HashSet[Rectangle]()
    def topRow(lst : List[Int]) : Int = lst.head
    def bottomRow(lst : List[Int]) : Int = lst.tail.head
    def leftCol(lst : List[Int]) : Int = lst.tail.tail.head
    def rightCol(lst : List[Int]) : Int = lst.tail.tail.tail.head



    val rows = matrix.length
    val cols = matrix(0).length


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

            //println("test")
            dp(bRow)(eRow)(bCol)(eCol) = newSet.toSet
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