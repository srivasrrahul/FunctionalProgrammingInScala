import scala.collection.mutable

object Solution {
  def lastStoneWeight(stones: Array[Int]): Int = {
    val pq = mutable.PriorityQueue.empty[Int]
    for (stone <- stones) {
      pq.addOne(stone)
    }

    def merge(w1 : Int,w2 : Int) : Option[Int] = {
      if (w1 == w2) {
        None
      }else {
        Some(w1-w2)
      }
    }
    while (pq.isEmpty == false && pq.size > 1) {
      val top1 = pq.dequeue()
      val top2 = pq.dequeue()
      val mergedStone = merge(top1,top2)
      mergedStone match {
        case Some(newStone) => pq.addOne(newStone)
        case _ => {}
      }

    }

    if (pq.size == 0) {
      0
    }else {
      pq.dequeue()
    }
  }
}