import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    val window = new mutable.TreeMap[Int,mutable.HashSet[Int]]()

    for (j <- 0 to k-1) {
      val defSet = window.getOrElseUpdate(nums(j),new mutable.HashSet[Int]())
      defSet.add(j)
    }

    var first = 0
    var last = k

    val res = new ArrayBuffer[Int]()

    while (last < nums.length) {
      val max = window.last
      res.addOne(max._1)

      val firstVal = nums(first)
      val remSet = window.get(firstVal).get
      if (remSet.size == 1) {
        window.remove(firstVal)
      }else {
        remSet.remove(first)
      }

      first = first + 1

      //now last
      val insertSet = window.getOrElseUpdate(nums(last),new mutable.HashSet[Int]())
      insertSet.add(last)
      last = last+1

    }
    val max = window.last
    res.addOne(max._1)
    res.toArray
  }
}