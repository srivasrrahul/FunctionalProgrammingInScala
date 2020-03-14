import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ServerId(row : Int,col : Int)

object Solution {
  def countServers(grid: Array[Array[Int]]): Int = {
    val rowSize = grid.length
    val colSize = grid(0).length
    val graph = new mutable.HashMap[ServerId, Int]()
    var j = 0
    var k = 0

    val rowsPopulated = new mutable.HashMap[Int, Int]()
    val colsPopulated = new mutable.HashMap[Int, Int]()
    for (j <- 0 to rowSize - 1) {
      for (k <- 0 to colSize - 1) {

        if (grid(j)(k) == 1) {
          //node exists
          val serverId = new ServerId(j, k)
          graph += ((serverId, 0))

          rowsPopulated.get(j) match {
            case Some(c) => {
              rowsPopulated += ((j, c + 1))
            }
            case None => {
              rowsPopulated += ((j, 1))
            }
          }

          colsPopulated.get(k) match {
            case Some(c) => {
              colsPopulated += ((k, c + 1))
            }
            case None => {
              colsPopulated += ((k, 1))
            }
          }
        }
      }
    }


    var commsServer = 0

    graph.foreach((serverId) => {

      val r = serverId._1.row
      val c = serverId._1.col

      rowsPopulated.get(r) match {
        case Some(c1) => {
          if (c1 == 1) {
            colsPopulated.get(c) match {
              case Some(c2) => {
                if (c2 == 1) {

                } else {
                  commsServer += 1
                }
              }
              case None => {
                //Not possible
                println("Error")
              }
            }
          } else {
            commsServer += 1
          }
        }
        case None => {
          println("Error")
        }
      }

    })

    commsServer
  }

  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Int](4,4)
    arr(0)(0) = 1
    arr(0)(1) = 1
    arr(1)(2) = 1
    arr(2)(2) = 1
    arr(3)(3) = 1

    println(countServers(arr))
  }
}

