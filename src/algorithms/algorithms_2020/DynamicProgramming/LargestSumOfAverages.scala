import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val currentIndex : Int,val groupPending : Int)
object Solution {
  def largestSumOfAverages(A: Array[Int], K: Int): Double = {
    val matrix = Array.ofDim[List[List[Int]]](K+1,A.length) //average and length
    val sumArr = new Array[Int](A.length)
    sumArr(0) = A(0)

    for (j <- 1 to A.length-1) {
      sumArr(j) = sumArr(j-1) + A(j)
    }

    def findSum(i1 : Int,i2 : Int)  : Int = {
      if (i1 == i2) {
        A(i1)
      }else {
        var totalSum = sumArr(i2)
        if (i1 > 0) {
          totalSum = totalSum - sumArr(i1-1)
        }

        totalSum
      }
    }
    for (k <- A.length-1 to 0 by -1) {
      matrix(1)(k) = List(List(k))
    }


    for (j <- 2 to K) {
      for (k <- A.length-j to 0 by -1) {
        val retValue = new ListBuffer[List[Int]]
        for (p <- A.length-j+1 to k+1 by -1) {
          val lsts = matrix(j-1)(p)
          for (lst <- lsts) {
            retValue.append(k::lst)
          }
        }

        matrix(j)(k) = retValue.toList
      }
    }

//    for (j <- 1 to matrix.length-1) {
//      println(matrix(j).mkString(","))
//    }

    def findAverage(lst : List[Int]) : Double = {
      var groupAverageSum = 0.0
      var prev = lst.head
      for (index <- lst.tail) {
        //Group is prev and index-1
        val currentGroupSum = findSum(prev,index-1)
        val currentSize = index-prev
        val average = currentGroupSum.toDouble/currentSize.toDouble
        groupAverageSum = groupAverageSum + average
        prev = index
      }

      val currentGroupSum = findSum(prev,A.length-1)
      val currentSize = A.length-1-prev+1
      val average = currentGroupSum.toDouble/currentSize.toDouble
      groupAverageSum = groupAverageSum + average
      groupAverageSum
    }

    var maxAverage = Double.MinValue

    for (j <- 1 to K) {
      val lsts = matrix(j)(0)
      //println(lsts)
      for (lst <- lsts) {

        val average = findAverage(lst)
        //println(lst + " " + average)
        if (average > maxAverage) {
          maxAverage = average
        }
      }

    }

    maxAverage


  }

  def main(args: Array[String]): Unit = {
    println(largestSumOfAverages(Array(9,1,2,3,9),3))
  }
}