import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Sorting
import util.control.Breaks._

object Solution {

  def minMeetingRooms(intervals: Array[Array[Int]]): Int = {
    intervals.length match {
      case 0 => 0
      case _ => {
        //val sortedIntervals = intervals.sortWith(_(0) < _(0))
        //val sortedIntervals = java.util.Arrays.sort(intervals)

        Sorting.quickSort(intervals)(new Ordering[Array[Int]] {
          override def compare(x: Array[Int], y: Array[Int]): Int = x(0) compareTo(y(0))
        } )
        val sortedIntervals = intervals
        //val sortedIntervals = intervals


        def ifOverlap(i1 : Array[Int],i2 : Array[Int]) : Boolean = {
          if (i1(1) > i2(0)) {
            true
          }else {
            false
          }
        }

        var maxDepth = 1

        for (j <- 1 to sortedIntervals.length-1) {
          //println("Current = " + sortedIntervals(j).mkString(","))
          //println(stack.mkString(","))
          var matchCount = 1 //at least with self
          for (k <- j-1 to 0 by -1) {
            if (ifOverlap(sortedIntervals(k),sortedIntervals(j))) {
              matchCount += 1
            }
          }

          if (matchCount > maxDepth) {
            maxDepth = matchCount
          }
        }

        maxDepth
      }
    }






  }

  def main(args: Array[String]): Unit = {
    println(minMeetingRooms(Array(Array(3,11),Array(4,9),Array(4,17),Array(9,10))))

  }
}