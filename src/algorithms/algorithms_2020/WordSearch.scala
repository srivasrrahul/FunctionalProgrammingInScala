import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class Index(val _x : Int,val _y : Int)

class Grid(val _arr : Array[Array[Char]]) {
  val arr = _arr
  val rowSize = arr.length
  val colSize = arr(0).length

  val indexMap = new mutable.HashMap[Char,mutable.HashSet[Index]]()

  for (j <- 0 to rowSize-1) {
    for (k <- 0 to colSize-1) {
      val ch = arr(j)(k)
      //println("j k " + j + " " + k + " " + ch)
      indexMap.get(ch) match {
        case Some(charIndexMap : mutable.HashSet[Index]) => {
          //println("Found Existing  for " + ch)
          charIndexMap.add(new Index(j,k))
          //println("Post Added  " + charIndexMap.size)
        }
        case None => {
          val charIndexMap = new mutable.HashSet[Index]()
          charIndexMap.add(new Index(j,k))
          indexMap += ((ch,charIndexMap))
        }
      }
    }
  }

  def findChar(char: Char) : List[Index] = {
    var indexes = new ListBuffer[Index]

    for (j <- 0 to rowSize - 1) {
      for (k <- 0 to colSize - 1) {
        if (arr(j)(k) == char) {
          indexes.addOne(new Index(j,k))
        }
      }
    }
    //}

    indexes.toList
  }

  def findNext(char: Char,index : Index) : List[Index] = {
    if (index == null) {
      findChar(char)
    }else {
      val indexes = new ListBuffer[Index]
      if (index._x + 1 < rowSize && arr(index._x + 1)(index._y) == char) {
        indexes.addOne(new Index(index._x + 1, index._y))
      }

      if (index._x - 1 >= 0 && arr(index._x - 1)(index._y) == char) {
        indexes.addOne(new Index(index._x - 1,index._y))
      }


      if (index._y + 1 < colSize && arr(index._x)(index._y + 1) == char) {
        indexes.addOne(new Index(index._x,index._y + 1))
      }

      if (index._y - 1 >= 0 && arr(index._x)(index._y - 1) == char) {
        indexes.addOne(new Index(index._x,index._y - 1))
      }

      indexes.toList
    }

  }
}
object Solution {
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    val grid = new Grid(board)

    def explore(currentIndex : Index,charIndex: Int,visitedLst : scala.collection.immutable.HashSet[Index]) : Boolean = {
      if (charIndex == word.length) {
        true
      }else {
        //println("Current index " + currentIndex)
        val neigbourLst = grid.findNext(word.charAt(charIndex),currentIndex)
        if (neigbourLst.isEmpty) {
          false
        }else {
          //println("for current " + currentIndex + " " + neigbourLst.mkString(","))
          var result = false
          breakable {
            neigbourLst.foreach(neigbour => {
              if (visitedLst.contains(neigbour) == false) {
                val isPathExists = explore(neigbour, charIndex + 1,visitedLst.+(neigbour))
                if (isPathExists == true) {
                  result = true
                  break
                }
              }
            })
          }

          result
        }
      }
    }

    //println(grid.findChar('S').mkString(","))

    explore(null,0,new scala.collection.immutable.HashSet[Index]())



  }

  def main(args: Array[String]): Unit = {
    val grid = Array(Array('A','B','C','E'),Array('S','F','C','S'),Array('A','D','E','E'))
    println(exist(grid,"ABCCED"))
  }
}