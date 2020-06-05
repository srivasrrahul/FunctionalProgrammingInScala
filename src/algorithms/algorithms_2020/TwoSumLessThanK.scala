import scala.collection.mutable

object Solution {
  def twoSumLessThanK(A: Array[Int], K: Int): Int = {
    val presence = new mutable.TreeSet[Int]()
    var found = false
    var retValue = -1
    for (j <- 0 to A.length-1) {
      val diff = K - A(j)
      val subTree = presence.rangeUntil(diff)
      //println(A(j) + " " + diff + " " + subTree)
      if (subTree.size > 0) {
        val localMaxValue = subTree.last + A(j)
        if (localMaxValue > retValue) {
          retValue = localMaxValue
        }
      }

      subTree.add(A(j))
    }

    retValue
  }

  def main(args: Array[String]): Unit = {
    println(twoSumLessThanK(Array(34,23,1,24,75,33,54,8),60))
  }
}