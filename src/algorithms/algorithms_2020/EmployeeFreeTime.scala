import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Definition for an Interval.
 * class Interval(var _start: Int, var _end: Int) {
 *   var start: Int = _start
 *  var end: Int = _end
 * }
 */

class Interval(var _start: Int, var _end: Int) {
  var start: Int = _start
  var end: Int = _end
}
case class GivenInterval(val begin : Int,val end : Int,val empId : Int = 0)

case class MinMaxTime(val beginTime : Int,val endTime : Int)

object Solution {

  def mergInterval(i1 : GivenInterval,i2 : GivenInterval) : List[GivenInterval] = {
    if (i2.begin < i1.end) {
      val min = i1.begin
      val max = if (i2.end > i1.end) i2.end else i1.end
      List(new GivenInterval(min,max))
    }else {
      List(i1,i2)
    }
  }
  def employeeFreeTime(schedule: List[List[Interval]]): List[Interval] = {
    var j = 0
    val arrBuffer = new mutable.ArrayBuffer[GivenInterval]()
    val minMaxTimeEmployees = new mutable.HashMap[Int,MinMaxTime]()
    var globalMinTime = Int.MaxValue
    var globalMaxTime = Int.MinValue

    for (perPersonSchedule <- schedule) {
      var minTime = Int.MaxValue
      var maxTime = Int.MinValue
      perPersonSchedule.foreach(interval => {
        val givenInterval = new GivenInterval(interval.start,interval.end,j)
        arrBuffer.addOne(givenInterval)

        if (interval.start < minTime) {
          minTime = interval.start
        }

        if (interval.end > maxTime) {
          maxTime = interval.end
        }

        if (interval.start < globalMinTime) {
          globalMinTime = interval.start
        }

        if (interval.end > globalMaxTime) {
          globalMaxTime = interval.end
        }
      })

      minMaxTimeEmployees += ((j,new MinMaxTime(minTime,maxTime)))

      j += 1
    }

    arrBuffer.sortInPlace()(new Ordering[GivenInterval] {
      override def compare(x: GivenInterval, y: GivenInterval): Int = {
        x.begin.compareTo(y.begin)
      }
    })

    val sortedGivenIntervals = arrBuffer.toArray

    val mergedIntervals = new ListBuffer[GivenInterval]

    mergedIntervals.addOne(sortedGivenIntervals(0))

    for (j <- 1 to sortedGivenIntervals.length-1) {
      val mergeInterval = mergInterval(mergedIntervals.last,sortedGivenIntervals(j))
      mergedIntervals.dropRightInPlace(1)
      mergedIntervals.addAll(mergeInterval)
    }

    val intervalResult = new ListBuffer[Interval]
    var prev = mergedIntervals.head

    mergedIntervals.tail.foreach(givenInterval => {
      if (prev.end < givenInterval.begin) {
        intervalResult.addOne(new Interval(prev.end,givenInterval.begin))
      }

      prev = givenInterval
    })

    intervalResult.toList
  }

  def main(args: Array[String]): Unit = {
//    val lst = List(List(new Interval(1,2),new Interval(5,6)),
//      List(new Interval(1,3)),
//      List(new Interval(4,10)))

    val lst = List(List(new Interval(1,3),new Interval(6,7)),
      List(new Interval(2,4)),
      List(new Interval(2,5),new Interval(9,12)))

    val res = employeeFreeTime(lst)
    for (l <- res) {
      println(l.start + "," + l.end)
    }
  }
}