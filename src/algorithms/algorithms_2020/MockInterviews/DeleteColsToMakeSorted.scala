import scala.collection.mutable
import scala.collection.immutable

case class Index(val s : Set[Int],val t : immutable.TreeSet[Int])
object Solution {
  def minDeletionSize(A: Array[String]): Int = {
    val rows = A.length
    val cols = A(0).length
    val sorted = new Array[Int](cols)

    sorted(0) = 1
    for (j <- 1 to cols-1) {
      var allowedMaxSorted = new mutable.ArrayBuffer[Int]
      for (k <- j-1 to 0 by -1) {
        var isLower = true
        for (p <- 0 to rows-1) {
          if (A(p)(k) > A(p)(j)) {
            isLower = false
          }
        }

        if (isLower) {
          val maxSorted = 1+sorted(k)
          allowedMaxSorted.append(maxSorted)
        }
      }

      if (allowedMaxSorted.isEmpty) {
        sorted(j) = 1
      }else {
        sorted(j) = allowedMaxSorted.max
      }
    }

    //println(sorted.mkString(","))
    cols - sorted.max
  }

  def main(args: Array[String]): Unit = {
    println(minDeletionSize(Array("pmtra","qbrns")))
  }
}
