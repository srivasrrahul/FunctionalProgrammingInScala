import scala.collection.mutable.ListBuffer

object Solution {
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    intervals.sortInPlace()(new Ordering[Array[Int]] {
      override def compare(x: Array[Int], y: Array[Int]): Int = {
        x(0).compare(y(0))
      }
    })

    val lst = new ListBuffer[Array[Int]]
    lst.addOne(intervals(0))

    def mergeNew(interval :Array[Int]) : Unit = {
      if (interval(0) <= lst.last(1)) {
        val remLast = lst.last
        lst.dropRightInPlace(1)
        lst.append(Array(remLast(0),math.max(remLast(1),interval(1))))
      }else {
        lst.append(interval)
      }
    }
    for (interval <- intervals.tail) {
      mergeNew(interval)
    }

    lst.toArray
  }
}