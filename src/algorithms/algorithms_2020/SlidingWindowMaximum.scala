import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Tuple(val index : Int,val indexValue : Int)

object TupleOrdering extends Ordering[Tuple] {
  override def compare(x: Tuple, y: Tuple): Int = {
    val compValue = x.indexValue.compareTo(y.indexValue)
    if (compValue == 0) {
      x.index.compareTo(y.index)
    }else {
      compValue
    }
  }
}
object Solution {
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    val lstBuffer = new ListBuffer[Tuple]
    val heap = mutable.PriorityQueue.empty[Tuple](TupleOrdering)

    for (j <- 0 to k-1) {
      val tuple = new Tuple(j,nums(j))
      lstBuffer.append(tuple)
      heap.addOne(tuple)
    }

    var begin = 0
    var end = k-1
    val retValue = new mutable.ArrayBuffer[Int]
    for (j <- k to nums.length-1) {

      while (heap.head.index < begin) {
        heap.dequeue()
      }
      //println(heap)

      retValue.append(heap.head.indexValue)
      val topList = lstBuffer.head
      lstBuffer.dropInPlace(0)

      if (topList.index == heap.head.index) {
        heap.dequeue()
      }



      val tuple = new Tuple(j,nums(j))
      lstBuffer.append(tuple)
      heap.addOne(tuple)
      begin = begin+1
      end = end + 1
    }

    while (heap.head.index < begin) {
      heap.dequeue()
    }
    retValue.append(heap.head.indexValue)
    retValue.toArray
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(9,10,9,-7,-4,-8,2,-6)
    println(arr.mkString(","))
    println(maxSlidingWindow(arr,5).mkString(","))
  }
}