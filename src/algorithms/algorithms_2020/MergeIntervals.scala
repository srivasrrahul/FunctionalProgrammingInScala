import scala.collection.mutable.ListBuffer

object Solution {
  def ifOveralap(i1 : Array[Int],i2: Array[Int]) : List[Array[Int]] = {
    val retValue = new ListBuffer[Array[Int]]
    if (i2(0) <= i1(1)) {
      //overlapping
      val intervalEnd = scala.math.max(i1(1),i2(1))
      retValue.addOne(Array(i1(0),intervalEnd))
    }else {
      retValue.addOne(i1)
      retValue.addOne(i2)
    }

    retValue.toList
  }
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    intervals.length match {
      case 0 => intervals
      case _ => {
        val sortedIntervals = intervals.sortWith(_(0) < _(0))
        var retValue = new ListBuffer[Array[Int]]()
        retValue.addOne(sortedIntervals(0))
        var prior = sortedIntervals(0)
        for (j <- 1 to sortedIntervals.length-1) {
          val current = sortedIntervals(j)
          val updatedInterval = ifOveralap(prior,current)
          retValue = retValue.dropRight(1)
          retValue.addAll(updatedInterval)
          prior = updatedInterval.last

        }
        retValue.toArray
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val interval = merge(Array(Array(1,4)))
    interval.foreach(arr => {
      println(arr.mkString(","))
    })

  }
}