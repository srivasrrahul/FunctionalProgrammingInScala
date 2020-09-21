import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
object Solution {
  def updateBoard(board: Array[Array[Char]], click: Array[Int]): Array[Array[Char]] = {
    val rows = board.length
    val cols = board(0).length

    def getNeigbours(index : Index) : List[Index] = {
      val x = index.x
      val y = index.y
      val lst = new ListBuffer[Index]
      if (x-1>=0) {
        lst.append(new Index(x-1,y))
      }

      if (x+1<rows) {
        lst.append(new Index(x+1,y))
      }

      if (y-1>=0) {
        lst.append(new Index(x,y-1))
      }

      if (y+1<cols) {
        lst.append(new Index(x,y+1))
      }

      if (x-1 >= 0 && y-1>=0) {
        lst.append(new Index(x-1,y-1))
      }

      if (x-1 >= 0 && y+1<cols ) {
        lst.append(new Index(x-1,y+1))
      }

      if (x+1 < rows && y-1>=0) {
        lst.append(new Index(x+1,y-1))
      }

      if (x+1 < rows && y+1 < cols) {
        lst.append(new Index(x+1,y+1))
      }

      lst.toList
    }


    def handleSafe(index : Index) : Unit = {
      board(index.x)(index.y) = 'B'
    }
    val visited = new mutable.HashSet[Index]()

    def reveal(index : Index) : Unit = {
      visited.add(index)
      board(index.x)(index.y) match {
        case 'M' => board(index.x)(index.y) = 'X' //lost
        case 'E' => {
          val lst = getNeigbours(index)
          var countNearMines = 0
          for (next <- lst) {
            if (board(next.x)(next.y) == 'M' || board(next.x)(next.y) == 'X') {
              countNearMines = countNearMines + 1
            }
          }

          //println(countNearMines + " " + lst)
          if (countNearMines > 0) {
            board(index.x)(index.y) = ('0' + countNearMines).toChar
          }else {
            handleSafe(index)
            for (next <- lst) {
              if (visited.contains(next) == false) {
                reveal(next)
              }

            }
          }
        }
        case _ => {

        }
      }
    }

    val index = new Index(click(0),click(1))
    reveal(index)
    board
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array('E', 'E', 'E', 'E', 'E'),
      Array('E', 'E', 'M', 'E', 'E'),
      Array('E', 'E', 'E', 'E', 'E'),
      Array('E', 'E', 'E', 'E', 'E'))

    val arr1 = updateBoard(arr,Array(3,0))
    for (j <- 0 to arr1.length-1) {
      println(arr1(j).mkString(","))
    }
  }
}