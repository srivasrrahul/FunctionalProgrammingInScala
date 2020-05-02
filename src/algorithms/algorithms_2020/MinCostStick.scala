import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def connectSticks(sticks: Array[Int]): Int = {
    val heap = new mutable.PriorityQueue[Int]()(Ordering[Int].reverse)
    for (elem <- sticks) {
      heap.addOne(elem)
    }

    var cost = 0
    while (heap.size >= 2) {

      val top = heap.dequeue()
      val top1 = heap.dequeue()
      //println(top + " " + top1)
      val newTop = top + top1
      cost = cost + newTop
      heap.addOne(newTop)
    }

    cost

  }

  def main(args: Array[String]): Unit = {

  }
}