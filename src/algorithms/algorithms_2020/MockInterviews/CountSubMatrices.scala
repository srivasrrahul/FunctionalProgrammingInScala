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


    def allOneRows(rowId : Int,rectangle: Rectangle) : Boolean = {
      var allOnes = true
      val lower = rectangle.topLeft.y
      val higher = rectangle.topRight.y

      val colSet = rowOnes.getOrElse(rowId,new mutable.TreeSet[Int]()).range(lower,higher+1)
      colSet.size == (higher-lower+1)
//      for (x <- rectangle.topLeft.y to rectangle.topRight.y) {
//        if (matrix(rowId)(x) != 1) {
//          allOnes = false
//        }
//      }
//      allOnes
    }

    def allOneCols(colId : Int,rectangle: Rectangle) : Boolean = {
      var allOnes = true
      val lower = rectangle.topLeft.x
      val higher = rectangle.bottomLeft.x
      val rows = colOnes.getOrElse(colId,new mutable.TreeSet[Int]()).range(lower,higher+1)
      rows == (higher-lower+1)
//      for (y <- rectangle.topLeft.x to rectangle.bottomLeft.x) {
//        if (matrix(y)(colId) != 1) {
//          allOnes = false
//        }
//      }
      //allOnes
    }

    def getValue(rectangle: Rectangle) : Int = {
      matrix(rectangle.topLeft.x)(rectangle.topLeft.y)
    }



    def leftColMerge(originalRectange : Rectangle,newRectange: Rectangle) : Rectangle = {
      val leftMostTopRow = newRectange.topLeft
      val leftMostBottomRow = newRectange.bottomLeft

      if (originalRectange.topLeft.y == leftMostTopRow.y-1) {
        //Can be mergged
        if (allOneCols(originalRectange.topLeft.y,originalRectange)) {
          new Rectangle(new Point(leftMostTopRow.x,leftMostTopRow.y-1),
                        newRectange.topRight,
                        new Point(leftMostBottomRow.x,leftMostTopRow.y-1),
            newRectange.bottomRight)
        }else {
          null
        }
      }else {
        null
      }
    }

    def rightColMerge(originalRectange : Rectangle,newRectange: Rectangle) : Rectangle = {
      val rightMostTopRow = newRectange.topRight
      val rightMostBottomRow = newRectange.bottomRight

      if (originalRectange.topRight.y == rightMostTopRow.y+1) {
        //Can be merged
        if (allOneCols(originalRectange.topRight.y,originalRectange)) {
          new Rectangle(newRectange.topLeft,newRectange.topRight.right(),newRectange.bottomLeft,newRectange.bottomRight.right())
        }else {
          null
        }
      }else {
        null
      }
    }


    def bottomRowMerge(originalRectange : Rectangle,newRectange: Rectangle) : Rectangle = {
      val bottomLeft = newRectange.bottomLeft
      val bottomRight = newRectange.bottomRight

      if (originalRectange.bottomLeft.x == bottomLeft.x+1) {
        //Can be merged
        if (allOneRows(originalRectange.bottomLeft.x,originalRectange)) {
          new Rectangle(newRectange.topLeft,newRectange.topRight,newRectange.bottomLeft.down(),newRectange.bottomRight.down())
        }else {
          null
        }
      }else {
        null
      }
    }

    def topRowMerge(originalRectange : Rectangle,newRectange: Rectangle) : Rectangle = {
      val topLeft = newRectange.topLeft
      val topRight = newRectange.topRight

      if (originalRectange.topLeft.x == topLeft.x-1) {
        //Can be merged
        if (allOneRows(originalRectange.topLeft.x,originalRectange)) {
          new Rectangle(newRectange.topLeft.up(),newRectange.topRight.up(),newRectange.bottomLeft,newRectange.bottomRight)
        }else {
          null
        }
      }else {
        null
      }
    }

    //val set = new mutable.HashSet[Rectangle]()


    val cache = new mutable.HashMap[Rectangle,Set[Rectangle]]()
    def countAllOnes(rectangle: Rectangle) : Set[Rectangle] = {
      if (rectangle.rowSize() == 1 && rectangle.colSize() == 1) {
        if (getValue(rectangle) == 1) {
          Set(rectangle)
        } else {
          Set()
        }
      }else {

        if (cache.contains(rectangle)) {
          cache.get(rectangle).get
        }else {
          val currentSet = new mutable.HashSet[Rectangle]()
          var isAllOne = false
          if (rectangle.colSize() > 1) {

            val l = countAllOnes(rectangle.removeLeftCol())
            currentSet.addAll(l)
            for (itr <- l) {
              val lMerged = leftColMerge(rectangle, itr)
              if (lMerged != null) {
                currentSet.add(lMerged)
              }
            }


            val rSet = countAllOnes(rectangle.removeRightCol())
            currentSet.addAll(rSet)
            for (r <- rSet) {
              val rMerged = rightColMerge(rectangle, r)
              if (rMerged != null) {
                currentSet.add(rMerged)
              }
            }
          }

          if (rectangle.rowSize() > 1) {
            val lSet = countAllOnes(rectangle.removeTopRow())
            currentSet.addAll(lSet)
            for (l <- lSet) {
              val lMerged = topRowMerge(rectangle, l)
              if (lMerged != null) {
                currentSet.add(lMerged)
              }
            }

            val rSet = countAllOnes(rectangle.removeBottomRow())
            currentSet.addAll(rSet)
            for (r <- rSet) {
              val rMerged = bottomRowMerge(rectangle, r)
              if (rMerged != null) {
                currentSet.add(rMerged)
              }
            }


          }


          //Is this all ones
          var allTopRowsOnes = allOneRows(rectangle.topLeft.x, rectangle)
          var allBottomRowOnes = allOneRows(rectangle.bottomLeft.x, rectangle)

          var allLeftColOnes = allOneCols(rectangle.topLeft.y, rectangle)
          val allRightColOnes = allOneCols(rectangle.topRight.y, rectangle)

          if (allTopRowsOnes && allBottomRowOnes && allLeftColOnes && allRightColOnes) {
            var isCurrentAllOne = false
            if (rectangle.rowSize() > 1) {
              val topRowRemoved = rectangle.removeTopRow()
              if (currentSet.contains(topRowRemoved)) {
                isCurrentAllOne = true
              }
            }

            if (isCurrentAllOne == false && rectangle.colSize() > 1) {
              val leftColRemoved = rectangle.removeLeftCol()
              if (currentSet.contains(leftColRemoved)) {
                isCurrentAllOne = true
              }
            }

            if (isCurrentAllOne) {
              currentSet.add(rectangle)
            }
          }

          val result = currentSet.toSet
          cache += ((rectangle, currentSet.toSet))
          result
        }
      }
    }

    val rows = matrix.length-1
    val cols = matrix(0).length-1
    val topLeft = new Point(0,0)
    val topRight = new Point(0,cols)
    val bottomLeft = new Point(rows,0)
    val bottomRight = new Point(rows,cols)
    val rectangle = new Rectangle(topLeft,topRight,bottomLeft,bottomRight)
    val set = countAllOnes(rectangle)
    set.size
  }
}