import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MedianFinder(val KIndex : Int) {

  /** initialize your data structure here. */
  val leftPQ = mutable.PriorityQueue.empty[Int] //leftPQ is of first k. This is max heap k,k-1,k-2
  //val rightPQ = mutable.PriorityQueue.empty[Int](Ordering[Int].reverse) //this is min heap k+1,k+2,k+3

  def addNum(num: Int) : Unit = {
    if (leftPQ.isEmpty) {
      leftPQ.addOne(num)
    }else {
      if (leftPQ.size < KIndex) {
        leftPQ.addOne(num)
      } else {
        val kthElement = leftPQ.head
        if (num < kthElement) {
          leftPQ.addOne(num)
          val topLeft = leftPQ.dequeue()
          //rightPQ.addOne(topLeft)
        }
      }
    }

  }

  def findK(): Int = {
    leftPQ.head
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
        medianFinder.addNum(k)
      }
    }

    medianFinder.findK()

    //arrBuffer(k-1)
  }
}