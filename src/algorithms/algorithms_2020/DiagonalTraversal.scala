import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object Solution {
  def findDiagonalOrder(matrix: Array[Array[Int]]): Array[Int] = {
    if (matrix.length == 0) {
      Array[Int]()
    }else {
      val maxRows = matrix.length - 1
      val maxCols = matrix(0).length - 1
      if (maxCols == 0 && maxRows == 0) {
        Array(matrix(0)(0))
      }else {

        //println(maxRows + " " + maxCols)
        var startTuple = (0, 0)

        def getNextTuple(): (Int, Int) = {
          var retValue = (-1,-1)
          if (startTuple._2 == 0) {
            retValue = (0, startTuple._1 + 1)
            startTuple = retValue
            if (retValue._2 > maxCols) {
              val diff = retValue._2 - maxCols
              retValue = (diff, maxCols)
            }
          } else {
            retValue = (startTuple._2 + 1, 0)
            startTuple = retValue
            if (retValue._1 > maxRows) {
              val diff = retValue._1 - maxRows
              retValue = (maxRows, diff)
            }
          }

          retValue
        }

        val retValue = new ArrayBuffer[Int]()
        retValue.append(matrix(0)(0))
        var direction = 1

        //var counter = 0

        var itr = startTuple
        breakable {
          while (true) {
            //counter = counter + 1
            itr = getNextTuple()

            //println(itr)

            if (itr._1 == maxRows && itr._2 == maxCols) {
              retValue.append(matrix(itr._1)(itr._2))
              break
            }
            if (direction == 1) {
              var j = itr._1
              var k = itr._2

              while (j <= maxRows && k >= 0) {
                retValue.append(matrix(j)(k))
                j = j + 1
                k = k - 1
              }

              direction = 0

            } else {
              var j = itr._1
              var k = itr._2

              while (j >= 0 && k <= maxCols) {
                retValue.append(matrix(j)(k))
                j = j - 1
                k = k + 1
              }

              direction = 1
            }


            //        if (counter == 10) {
            //          break()
            //        }

          }
        }

        retValue.toArray
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array[Array[Int]]()// ,Array(4,5,6),Array(7,8,9)
    println(findDiagonalOrder(matrix).mkString(","))
  }
}