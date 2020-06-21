import scala.collection.mutable

object Solution {
  def sumSubarrayMins(A: Array[Int]): Int = {

    val treeMap = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()
    for (j <- 0 to A.length-1) {
      val defaultSet = treeMap.getOrElseUpdate(A(j),new mutable.TreeSet[Int]())
      defaultSet.add(j)
    }

    //println(treeMap)
    def getMin(j : Int,k : Int) : Int = {
      val min = math.min(A(j),A(k))
      //val max = math.max(A(k),A(j))

      val range = treeMap.rangeUntil(min+1)
      //println("For " + j + " " + k + " " + range)
      var minValFound = min
      var minFound = false
      for ((minCurrent,minIndexes) <- range if minFound == false)  {
        val indexRange = minIndexes.range(j,k+1)
        //println("For " + j + " " + k + " " + range + " "  + indexRange)
        if (indexRange.size > 0) {
          if (minCurrent < minValFound) {
            minValFound = minCurrent
            minFound = true
          }
        }
      }

      minValFound

    }
    var sum = 0
    val modVal = Math.pow(10,9).toInt + 7
    for (j <- 0 to A.length-1) {
      for (k <- j to A.length-1) {
        var minArr = 0
        if (j == k) {
          minArr = A(j)
        }else {
          minArr = getMin(j,k)
        }

        println("From " + j + " " + k + " => "+  minArr)
        sum = (sum + minArr) % modVal
      }
    }

    sum
  }

  def main(args: Array[String]): Unit = {
    println(sumSubarrayMins(Array(3,1,2,4)))
  }
}