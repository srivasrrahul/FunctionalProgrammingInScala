
object Solution {
  def shortestToChar(s: String, c: Char): Array[Int] = {
    val t = 0
    val retValue = new Array[Int](s.length)
    val indexMap = new scala.collection.mutable.TreeSet[Int]

    for (j <- 0 to s.length-1) {
      val ch = s(j)
      if (ch == c) {
        indexMap.add(j)
      }
    }

    for (j <- 0 to s.length-1) {
      val ch = s(j)
      if (ch == c) {
        retValue(j) = 0
      }else {
        val rightSide = leftSide.rangeFrom(j)
        val leftSide = rightSide.rangeUntil(j)

        var minDiff = Int.MaxValue

        if (rightSide.size > 0) {
          val currentDiff = math.abs(indexMap.head - j)
          if (currentDiff < minDiff) {
            minDiff = currentDiff
          }
        }

        if (leftSide.size > 0) {
          val currentDiff = math.abs(indexMap.last-j)
          if (currentDiff < minDiff) {
            minDiff = currentDiff
          }
        }

        retValue(j) = minDiff
      }


    }

    retValue
  }
}