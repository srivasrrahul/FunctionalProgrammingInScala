import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MedianFinder(val KIndex : Int) {

  /** initialize your data structure here. */
  val leftPQ = new mutable.TreeMap[Int,Int]() //leftPQ is unique elements smaller than k. This is max heap k,k-1,k-2
  var leftCount = 0

  def addNum(num: Int) : Unit = {
    if (leftCount < KIndex) {
      val defaultCount = leftPQ.getOrElseUpdate(num,0)
      leftPQ += ((num,defaultCount+1))
      leftCount = leftCount+1
    }else {
      val (largestElement,largestCount) = leftPQ.last
      if (num > largestElement) {
        //ignore
      }else {

        if (num == largestElement) {
          //Do nothing
        }else {
          //num < largestElement
          val defaultCount = leftPQ.getOrElseUpdate(num,0)
          leftPQ += ((num,defaultCount+1))

          if (largestCount == 1) {
            leftPQ.remove(largestElement)
          }else {
            leftPQ += ((largestElement,largestCount-1))
          }
        }
      }
    }

  }

  def findK(): Int = {
    var alreadyFound = 0
    var retValue = -1
    for ((key,count) <- leftPQ if retValue == -1) {
      alreadyFound = alreadyFound + count
      if (alreadyFound <= KIndex) {
        retValue = key
      }
    }

    retValue
  }

  def debug() : Unit = {
    println(leftPQ)
    //println(rightPQ)

  }

}

object Solution {
  def smallestDistancePair(nums: Array[Int], k: Int): Int = {
    val medianFinder = new MedianFinder(k)
    val arrBuffer = new ArrayBuffer[Int]()
    for (j <- 0 to nums.length-1) {
      for (k <- j+1 to nums.length-1) {
        val diff = math.abs(nums(j)-nums(k))
        medianFinder.addNum(diff)
      }
    }

    medianFinder.findK()

    //arrBuffer(k-1)
  }
}