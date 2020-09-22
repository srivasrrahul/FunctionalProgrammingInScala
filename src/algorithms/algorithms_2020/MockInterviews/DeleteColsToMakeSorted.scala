import scala.collection.mutable
import scala.collection.immutable

case class Index(val s : Set[Int],val t : immutable.TreeSet[Int])
object Solution {
  def minDeletionSize(A: Array[String]): Int = {
    def checkSorted(j : Int,pendingCols : immutable.TreeSet[Int]) : Boolean = {
      if (pendingCols.size == 1) {
        true
      }else {
        var sorted = true
        var prev = pendingCols.head
        for (t <- pendingCols.tail if sorted == true) {
          if (A(j)(t) < A(j)(prev)) {
            sorted = false
          }else {
            prev = t
          }
        }

        sorted
      }
    }

    def checkAllSorted(s : Set[Int], p : immutable.TreeSet[Int]) : Boolean = {
      var allSorted = true
      for (j <- s if allSorted == true) {
        if (checkSorted(j,p) == false) {
          allSorted = false
        }
      }

      allSorted
    }

    val cache = new mutable.HashMap[Index,Int]()
    def itr(unsortedRows : Set[Int],pendingCols : immutable.TreeSet[Int]) : Int = {
      if (unsortedRows.size == 0 || pendingCols.size == 1 || checkAllSorted(unsortedRows,pendingCols)) {
        0
      }else {
        val index = new Index(unsortedRows,pendingCols)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var globalCount = Int.MaxValue
          for (j <- pendingCols) {
            val colLst = pendingCols.-(j)
            val sortedlst = new mutable.HashSet[Int]()
            for (k <- unsortedRows) {
              if (checkSorted(k, colLst)) {
                sortedlst.add(k)
              }
            }

            val count = 1 + itr(unsortedRows.diff(sortedlst.toSet), colLst)
            if (count < globalCount) {
              globalCount = count
            }
          }

          cache += ((index,globalCount))
          globalCount
        }
      }
    }

    itr(Range(0,A.length).toSet,new immutable.TreeSet[Int].++(Range(0,A(0).length).toSet))
  }
}