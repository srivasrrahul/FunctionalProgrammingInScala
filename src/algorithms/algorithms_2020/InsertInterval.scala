import scala.collection.mutable.ArrayBuffer

object Solution {
  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    def ifOverlap(i1 : Array[Int],i2 : Array[Int]) : Boolean = {
      if (i1(0) <= i2(0)) {
       (i1(0) <= i2(0) && i1(1) >= i2(0))
      }else {
        ifOverlap(i2,i1)
      }
    }
    def findOverlapping(begin : Int,end : Int) : Option[(Int,Int)] = {
      if (begin == end) {
        if (ifOverlap(intervals(begin),newInterval)) {
          Some((begin,begin))
        }else {
          None
        }
      }else {
        val mid = begin + (end-begin)/2

        var overlappingInterval : Option[(Int,Int)] = None
        if (ifOverlap(intervals(mid),newInterval)) {
          //println("here")
          val furtherLeft = findOverlapping(begin,mid)
          furtherLeft match {
            case Some((x1,x2)) => {
              overlappingInterval = Some((x1,mid))
            }
            case None => {
              overlappingInterval = Some((mid,mid))
            }
          }


          val furtherRight = findOverlapping(mid+1,end)
          furtherRight match {
            case None => {

            }
            case Some((y1,y2)) => {
              overlappingInterval = Some((overlappingInterval.get._1,y2))
            }
          }

          overlappingInterval

        }else {
          //Is not overlapping
          if (intervals(mid)(0) < newInterval(0)) {
            findOverlapping(mid+1,end)
          }else {
            findOverlapping(begin,mid)
          }
        }

      }
    }

    if (intervals.isEmpty) {
      Array(newInterval)
    }else {
      val overlappedInterval = findOverlapping(0, intervals.length - 1)
      println(overlappedInterval)
      overlappedInterval match {
        case None => {
          val retValue = new ArrayBuffer[Array[Int]]()
          var added = false
          for (j <- 0 to intervals.length - 1) {
            if (intervals(j)(0) < newInterval(0) || added == true) {
              retValue.append(intervals(j))
            } else {
              added = true
              retValue.append(newInterval)
              retValue.append(intervals(j))
            }
          }


          if (added == false) {
            retValue.append(newInterval)
          }

          retValue.toArray
        }
        case Some((begin, end)) => {
          val retValue = new ArrayBuffer[Array[Int]]()
          val left = intervals.take(begin)
          val (_, right) = intervals.splitAt(end + 1)

          var minX = math.min(intervals(begin)(0), newInterval(0))
          var maxY = math.max(intervals(begin)(1), newInterval(1))
          for (k <- begin + 1 to end) {
            if (intervals(k)(1) > maxY) {
              maxY = intervals(k)(1)
            }
          }


          retValue.appendAll(left)
          retValue.append(Array(minX, maxY))
          retValue.appendAll(right)

          retValue.toArray

        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(1,2),Array(3,5),Array(6,7),Array(8,10),Array(12,16))
    val res = insert(Array(Array(10,12)),Array(11,13))
    for (x <- res) {
      println(x.mkString(","))
    }
  }
}