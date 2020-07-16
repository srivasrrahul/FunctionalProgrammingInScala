
import scala.collection.mutable

class MedianFinder() {

  /** initialize your data structure here. */
  val leftPQ = mutable.PriorityQueue.empty[Int]
  val rightPQ = mutable.PriorityQueue.empty[Int](Ordering[Int].reverse)

  def addNum(num: Int) : Unit = {
    if (leftPQ.isEmpty) {
      leftPQ.addOne(num)
    }else {
      if (leftPQ.size > rightPQ.size) {
        //need to rebalance
        val topLeft = leftPQ.head
        if (num >= topLeft) {
          //no need to rebalance
          rightPQ.addOne(num)
        }else {
          leftPQ.addOne(num)
          val topLeftUpdated = leftPQ.dequeue()
          rightPQ.addOne(topLeftUpdated)
        }
      }else {
        //both are equal
        //left needs to have more at the end
        val topLeft = leftPQ.head
        val topRight = rightPQ.head
        if (num <= topLeft) {
          leftPQ.addOne(num)
        }else {
          rightPQ.addOne(num)
        }

        val leftSize = leftPQ.size
        val rightSize = rightPQ.size
        if (leftSize < rightSize) {
          val topRight = rightPQ.dequeue()
          leftPQ.addOne(topRight)
        }
      }
    }
  }

  def findMedian(): Double = {
    (leftPQ.headOption,rightPQ.headOption) match {
      case (None,_) => 0.0
      case (Some(topLeft),None) => topLeft
      case (Some(topLeft),Some(topRight)) => {
        val leftSize = leftPQ.size
        val rightSize = rightPQ.size
        if (leftSize > rightSize) {
          topLeft
        }else {
          //println("Here")
          (topLeft.toDouble+topRight.toDouble)/2.0
        }
      }
    }
  }

  def debug() : Unit = {
    println(leftPQ)
    println(rightPQ)

  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val medianFinder = new MedianFinder
    medianFinder.findMedian()
    medianFinder.addNum(1)
    println(medianFinder.findMedian())
    medianFinder.addNum(2)
    medianFinder.debug()
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    medianFinder.debug()
    println(medianFinder.findMedian())
    medianFinder.addNum(0)
    medianFinder.debug()
  }
}
