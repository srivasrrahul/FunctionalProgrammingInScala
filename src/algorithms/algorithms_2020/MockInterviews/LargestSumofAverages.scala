import scala.collection.mutable.ListBuffer

object Solution {
  def largestSumOfAverages(A: Array[Int], K: Int): Double = {
    def itr(j : Int) : List[List[(Int,Int)]] = {
      if (j == 0) {
        List(List((A(j),1)))
      }else {
        val groups = itr(j-1)
        //two ways
        //add to one of each group
        //or create a new group

        val lstBuffer = new ListBuffer[List[(Int,Int)]]
        for (group <- groups) {
          val last = group.head
          //either update the same group
          val newTotal = last._1+A(j)
          val newCount = last._2+1
          lstBuffer.append((newTotal,newCount) :: group.tail)
          if (group.size < K) {
            //add a new group starting from here
            lstBuffer.append((A(j), 1) :: group)
          }
        }

        lstBuffer.toList

      }
    }

    val lsts = itr(A.length-1)
    var maxAverage = 0.0
    for (lst <- lsts) {
      var totalAverage = 0.0
      for ((total,elementCount) <- lst) {
        val avg = total.toDouble/elementCount.toDouble
        totalAverage = totalAverage + avg
      }

      if (totalAverage > maxAverage) {
        maxAverage = totalAverage
      }
    }

    maxAverage
  }
}