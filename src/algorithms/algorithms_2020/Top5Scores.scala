import scala.collection.mutable

object Solution {
  def highFive(items: Array[Array[Int]]): Array[Array[Int]] = {
    val orderedSet = new mutable.HashMap[Int,mutable.PriorityQueue[Int]]()

    for (item <- items) {
      val existinSet = orderedSet.getOrElseUpdate(item(0),mutable.PriorityQueue.empty[Int])
      existinSet.addOne(item(1))
    }

    val arrBuffer = new scala.collection.mutable.ArrayBuffer[Array[Int]]
    for ((id,marks) <- orderedSet) {
      val top5Marks = new Array[Int](5)
      top5Marks(0) = marks.dequeue()
      top5Marks(1) = marks.dequeue()
      top5Marks(2) = marks.dequeue()
      top5Marks(3) = marks.dequeue()
      top5Marks(4) = marks.dequeue()

      val average = top5Marks.sum/5
      arrBuffer.append(Array(id,average))
    }

    arrBuffer.sortInPlace()(new Ordering[Array[Int]] {
      override def compare(x: Array[Int], y: Array[Int]): Int = {
        x(0).compareTo(y(0))
      }
    })

    arrBuffer.toArray
  }

  def main(args: Array[String]): Unit = {

  }
}