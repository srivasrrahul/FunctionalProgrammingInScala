import scala.collection.mutable

object LessThan extends Ordering[Int] {
  override def compare(x: Int, y: Int): Int = {
    y.compare(x)
  }
}
class KthLargest(_k: Int, _nums: Array[Int]) {

  val max_heap = new mutable.PriorityQueue[Int]()(LessThan)

  def upsert(e : Int): Int = {
    val max_val = max_heap.head
    //println("Max Val is  " + max_val)
    if (e >= max_val) {

      max_heap.dequeue()
      max_heap.addOne(e)

    }

    max_heap.head
  }


  val k = _k
  for (e <- _nums) {
    if (max_heap.size >= k) {
      upsert(e)
    }else {
      //println("Adding " + e)
      max_heap.addOne(e)
    }

  }

  def add(v : Int): Int = {
    if (max_heap.size < k) {
      max_heap.addOne(v)
      max_heap.head
    }else {
      upsert(v)
    }

  }



}

object Solution {
  def main(args: Array[String]): Unit = {
    val s = new KthLargest(3,Array(4,5,8,2))
    println("====")
    println("Adding 3 => " + s.add(3))
    println("Adding 5 => " + s.add(5))
    println("Adding 10 => " + s.add(10))
    println("Adding 9 => " + s.add(9))
    println("Adding 4 => " + s.add(4))

  }
}