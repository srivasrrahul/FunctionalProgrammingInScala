import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def totalNQueens(n: Int): Int = {
    def givePossibleStates(prevStates : List[Int]) : List[Int] = {
      val possibleSet = new mutable.HashSet[Int]()
      for (j <- 0 to n-1) {
        possibleSet.add(j)
      }

      var rowIndex = 0

      for (prevCol <- prevStates.reverse) {
        //remove that col
        possibleSet.remove(prevCol)
        val rowDiff = math.abs(prevStates.size - rowIndex)

        //remove rightDiagonal
        possibleSet.remove(prevCol + rowDiff)
        //remove leftDiagonal
        possibleSet.remove(prevCol - rowDiff)
        rowIndex = rowIndex+1
      }

      possibleSet.toList
    }

    var count = 0
    def itr(rowIndex : Int,prevState : List[Int]) : Unit = { //prevstate gives queen placed in earlier cols
      //println("For row " + rowIndex + " " + prevState)
      if (rowIndex == n-1) {
        val pending = givePossibleStates(prevState)
        //println(pending)
        if (pending.size == 1) {
          count = count+1
        }
      }else {
        val pendingCols = givePossibleStates(prevState)
        for (pendingCol <- pendingCols) {
          itr(rowIndex+1,pendingCol :: prevState)
        }
      }
    }

    itr(0,List())
    //println(solutions.length)
    count

  }

  def main(args: Array[String]): Unit = {
    println(totalNQueens(4))
  }
}