import scala.collection.mutable.ListBuffer

object Solution {
  def largestSumOfAverages(A: Array[Int], K: Int): Double = {
    def splitArrInGroups() : List[List[Int]] = {
      if (K == 1) {
        List(List(0))
      }else {
        def itr(currentIndex : Int,groupPending : Int) : List[List[Int]] = {
          if (currentIndex == A.length-1) {
            val retValue = new ListBuffer[List[Int]]
            if (groupPending >= 1) {
              retValue.append(List(currentIndex))

            }else {
              //Not valid combination
            }

            retValue.toList
          }else {
            if (groupPending == 0) {
              List() //Not valud combination
            }else {
              val retValue = new ListBuffer[List[Int]]
              val currentGroupStartsWith = List(currentIndex)
              for (j <- currentIndex+1 to A.length-1) {
                val currentGroupSize = j-currentIndex
                val pendingElementSize = A.length-1-j
                val next = itr(j,groupPending-1)
                if (next.isEmpty == false) {
                  for (nextLst <- next) {
                    retValue.append(currentGroupStartsWith ++ nextLst)
                  }
                }
                retValue.append(currentGroupStartsWith)

              }

              retValue.toList
            }
          }
        }

        itr(0,K)
      }
    }

    val lst = splitArrInGroups()
    val sumArr = new Array[Int](A.length)
    sumArr(0) = A(0)
    for (j <- 1 to sumArr.length-1) {
      sumArr(j) = sumArr(j-1) + A(j)
    }

    def findSum(i1 : Int,i2 : Int) : Int = {
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
    var maxAvg = 0.0
    for (group <- lst) {

      val groupAvg = findAverage(group)
      println(group + " " + groupAvg)
      if (groupAvg > maxAvg) {
        maxAvg = groupAvg
      }
    }
    //println(lst.mkString("\n"))
    maxAvg
  }

  def main(args: Array[String]): Unit = {
    println(largestSumOfAverages(Array(9,1,2,3,9),3))
  }
}