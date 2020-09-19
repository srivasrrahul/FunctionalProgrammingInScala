import scala.collection.mutable

object Solution {
  def minDeletionSize(A: Array[String]): Int = {
    val rows = A.length
    val cols = A(0).length
    val colSet = Range(0,cols).toSet
    def checkSorted(d : Set[Int]) : Boolean = {
      val trueCols = colSet.diff(d)
      var ifSorted = true

      for (j <- 0 to rows-1 if ifSorted == true) {
        var prev = ('a' - 1).toChar
        for (k <- 0 to cols - 1 if ifSorted == true) {
          if (d.contains(k) == false) {
            if (A(j)(k) < prev) {
              ifSorted = false
            } else {
              prev = A(j)(k)
            }
          }
        }

      }

      ifSorted
    }

    val cache = new mutable.HashMap[Set[Int],Int]()
    def itr(d : Set[Int]) : Int = {
      if (checkSorted(d)) {
        d.size
      }else {
        if (cache.contains(d)) {
          cache.get(d).get
        }else {
          var minDeletion = Int.MaxValue
          for (j <- 0 to cols - 1) {
            if (d.contains(j) == false) {
              //col not removed. lets remove it
              val option1 = itr(d.+(j))
              if (option1 < minDeletion) {
                minDeletion = option1
              }
            }
          }

          cache += ((d,minDeletion))
          minDeletion
        }

      }
    }

    itr(Set())
  }
}