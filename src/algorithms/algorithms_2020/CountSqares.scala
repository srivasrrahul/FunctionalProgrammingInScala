import scala.collection.mutable

case class Index(val x : Int,val y : Int)
object Solution {
  def countSquares(matrix: Array[Array[Int]]): Int = {
    val indexes = new mutable.HashMap[Index,Array[Int]]()
    val rowSize = matrix.length
    val colSize = matrix(0).length

    val iterationSize = scala.math.min(rowSize,colSize)

    for (j <- 0 to matrix.length-1) {
      for (k <- 0 to matrix(0).length-1) {
        indexes += ((new Index(j,k),new Array[Int](iterationSize)))
      }
    }

    var countOfSquare = 0
    for (i <- 1 to iterationSize) {
      for (j <- 0 to rowSize-1) {
        for (k <- 0 to colSize-1) {
          i match {
            case 1 => {
              if (matrix(j)(k) == 1) {
                val index = indexes.getOrElseUpdate(new Index(j,k),null)
                index(0) = 1
                countOfSquare +=1
              }
            }
            case _ => {
              if (matrix(j)(k) == 1) {
                val index = indexes.getOrElseUpdate(new Index(j,k),null)
                if (j+1 < rowSize && k+1 < colSize) {
                  val indexRight = indexes.getOrElseUpdate(new Index(j,k+1),null)
                  val indexBelow = indexes.getOrElseUpdate(new Index(j+1,k),null)
                  val indexDiag= indexes.getOrElseUpdate(new Index(j+1,k+1),null)
                  if (indexRight(i-2) == 1 && indexBelow(i-2) == 1 && indexDiag(i-2) == 1) {
                    index(i-1) = 1
                    countOfSquare +=1
                  }
                }
              }
            }
          }
        }
      }


    }

    countOfSquare

  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(0,0,0),Array(0,1,0),Array(0,1,0),Array(1,1,1),Array(1,1,0))
    println(countSquares(arr))
  }
}