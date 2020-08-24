import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Rectangle(val rowBegin : Int,val rowEnd : Int,val colBegin : Int,val colEnd : Int) {
  def rowSize() : Int = {
    rowEnd-rowBegin+1
  }
  def colSize() : Int = {
    colEnd-colBegin+1
  }

  def incRowBegin() : Rectangle = {
    new Rectangle(rowBegin+1,rowEnd,colBegin,colEnd)
  }

  def decRowBegin() : Rectangle = {
    new Rectangle(rowBegin-1,rowEnd,colBegin,colEnd)
  }

  def incRowEnd() : Rectangle = {
    new Rectangle(rowBegin,rowEnd+1,colBegin,colEnd)
  }

  def decRowEnd() : Rectangle = {
    new Rectangle(rowBegin,rowEnd-1,colBegin,colEnd)
  }

  def incColBegin() : Rectangle = {
    new Rectangle(rowBegin,rowEnd,colBegin+1,colEnd)
  }

  def decColBegin() : Rectangle = {
    new Rectangle(rowBegin,rowEnd,colBegin-1,colEnd)
  }

  def incColEnd() : Rectangle = {
    new Rectangle(rowBegin,rowEnd,colBegin,colEnd+1)
  }

  def decColEnd() : Rectangle = {
    new Rectangle(rowBegin,rowEnd,colBegin,colEnd-1)
  }

  def area() : Int = {
    (rowEnd-rowBegin+1)*(colEnd-colBegin+1)
  }


}
object Solution {

  def maximalRectangle(matrix: Array[Array[Char]]): Int = {

    if (matrix.isEmpty) {
      0
    }else {
      val maxRows = matrix.length - 1
      val maxCols = matrix(0).length - 1

      val cache = new mutable.HashMap[Rectangle, Rectangle]()
      val colsIndex = new mutable.HashMap[Int,mutable.TreeSet[Int]]()
      val rowsIndex = new mutable.HashMap[Int,mutable.TreeSet[Int]]()

      for (j <- 0 to maxRows) {
        for (k <- 0 to maxCols) {
          if (matrix(j)(k) == '1') {
            val defaultRowSet = rowsIndex.getOrElseUpdate(j,new mutable.TreeSet[Int]())
            defaultRowSet.add(k)

            val defaultColsSet = colsIndex.getOrElseUpdate(k,new mutable.TreeSet[Int]())
            defaultColsSet.add(j)
          }
        }
      }

      //println(rowsIndex)
      //println(colsIndex)
      def allRowOnes(rowId: Int, result: Rectangle): Boolean = {
        val rowSet = rowsIndex.getOrElse(rowId,new mutable.TreeSet[Int]())
        //var mock = false
        if (rowSet.range(result.colBegin,result.colEnd+1).size == result.colSize()) {
          true
        }else {
          false
        }
//        var allRowOnes = true
//        for (j <- result.colBegin to result.colEnd) {
//          if (matrix(rowId)(j) != '1') {
//            allRowOnes = false
//          }
//        }
//
//        if (mock != allRowOnes) {
//          println(rowId + " " + result)
//        }
//        allRowOnes
      }

      def allColOnes(colId: Int, result: Rectangle): Boolean = {
//        var allColOnes = true
//        for (j <- result.rowBegin to result.rowEnd) {
//          if (matrix(j)(colId) != '1') {
//            allColOnes = false
//          }
//        }
//
//        allColOnes
        val cols = colsIndex.getOrElse(colId,new mutable.TreeSet[Int]())
        val colsSet = cols.range(result.rowBegin,result.rowEnd+1)
        if (colsSet.size == result.rowSize()) {
          true
        }else {
          false
        }
      }


      def itr(rectangle: Rectangle): Rectangle = {
        //println(rectangle)
        if (rectangle.rowSize() == 1 && rectangle.colSize() == 1) {
          if (matrix(rectangle.rowBegin)(rectangle.colBegin) == '1') {
            rectangle
          } else {
            null
          }
        } else {



          if (cache.contains(rectangle)) {
            //println("Hit ")
            cache.get(rectangle).get
          } else {
            var r1: Rectangle = null
            if (rectangle.rowBegin + 1 <= rectangle.rowEnd) {
              val r1Temp = itr(rectangle.incRowBegin())
              if (r1Temp != null && rectangle.rowBegin == r1Temp.rowBegin - 1 && allRowOnes(rectangle.rowBegin, r1Temp)) {
                r1 = r1Temp.decRowBegin()
              } else {
                r1 = r1Temp
              }
            }


            var r2: Rectangle = null
            if (rectangle.rowEnd - 1 >= rectangle.rowBegin) {
              val r2Temp = itr(rectangle.decRowEnd())
              if (r2Temp != null && rectangle.rowEnd == r2Temp.rowEnd + 1 && allRowOnes(rectangle.rowEnd, r2Temp)) {
                r2 = r2Temp.incRowEnd()
              } else {
                r2 = r2Temp
              }
            }


            var r3: Rectangle = null
            if (rectangle.colBegin + 1 <= rectangle.colEnd) {
              val r3Temp = itr(rectangle.incColBegin())
              if (r3Temp != null && rectangle.colBegin == r3Temp.colBegin - 1 && allColOnes(rectangle.colBegin, r3Temp)) {
                r3 = r3Temp.decColBegin()
              } else {
                r3 = r3Temp
              }
            }

            var r4: Rectangle = null
            if (rectangle.colEnd - 1 >= rectangle.colBegin) {
              val r4Temp = itr(rectangle.decColEnd())
              if (r4Temp != null && rectangle.colEnd == r4Temp.colEnd + 1 && allColOnes(rectangle.colEnd, r4Temp)) {
                r4 = r4Temp.incColEnd()
              } else {
                r4 = r4Temp
              }
            }

            val area = new ArrayBuffer[Rectangle]()
            if (r1 != null) {
              area.append(r1)
            }

            if (r2 != null) {
              area.append(r2)
            }

            if (r3 != null) {
              area.append(r3)
            }

            if (r4 != null) {
              area.append(r4)
            }

            if (area.size > 0) {
              val retValue = area.max(new Ordering[Rectangle] {
                override def compare(x: Rectangle, y: Rectangle): Int = {
                  x.area().compareTo(y.area())
                }
              })

              //println(" For rectangle " + rectangle + " " + retValue + " " + retValue.area())
              cache += ((rectangle, retValue))
              retValue
            } else {
              //println(" For rectangle " + rectangle + " " + " zero ")
              cache += ((rectangle, null))
              null
            }
          }
        }
      }


      val retValue = itr(new Rectangle(0, maxRows, 0, maxCols))
      if (retValue != null) {
        //println(cache)
        retValue.area()
      } else {
        0
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array(Array('1','0','1','0','0'),
      Array('1','0','1','1','1'),
      Array('1','1','1','1','1'),
      Array('1','0','0','1','0'))
    val a1 = Array(Array('1','1','1'),Array('1','1','1'))
    println(maximalRectangle(array))
  }
}