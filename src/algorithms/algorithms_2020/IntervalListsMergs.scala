import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Sorting

object Solution {
  def mergeList(i1 : Array[Int],i2 : Array[Int]) : (List[Array[Int]],Array[Int]) = {
    //println(i1.mkString(",")  +  " " + i2.mkString(","))
    val retValue = new ListBuffer[Array[Int]]
    if (i1(1) >= i2(0)) {
      val min = i1(0)
      val max = if (i2(1) > i1(1)) i2(1) else i1(1)
      retValue.addOne(Array(min,max))
      //println("Overlap")
      val minExtension = if (i2(1) > i1(1)) i1(1) else i2(1)
      (retValue.toList,Array(i2(0),minExtension))
    }else {
      retValue.addOne(i1)
      retValue.addOne(i2)
      (retValue.toList,Array())
    }
  }
  def intervalIntersection(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
    var combinedIntervalsBuffer = new ArrayBuffer[Array[Int]](A.length + B.length)

    combinedIntervalsBuffer.addAll(A)
    combinedIntervalsBuffer.addAll(B)

    val combinedIntervals = combinedIntervalsBuffer.toArray

    Sorting.quickSort(combinedIntervals)(new Ordering[Array[Int]] {
      override def compare(x: Array[Int], y: Array[Int]): Int = {
        x(0).compareTo((y(0)))
      }
    })

//    for (c <- combinedIntervals) {
//      println(c.mkString(","))
//    }

    val mergedIntervals = new ListBuffer[Array[Int]]
    val mergersPerformed = new ArrayBuffer[Array[Int]]

    if (combinedIntervals.length > 0) {
      mergedIntervals.addOne(combinedIntervals(0))

      for (j <- 1 to combinedIntervals.length - 1) {
        val current = combinedIntervals(j)
        val (possibleMerge, anyMerger) = mergeList(mergedIntervals.last, current)
        if (anyMerger.length > 0) {
          mergersPerformed.addOne(anyMerger)
          mergedIntervals.dropRightInPlace(1)
          mergedIntervals.addAll(possibleMerge)
        } else {
          mergedIntervals.addOne(current)
        }
      }

      mergersPerformed.toArray
    }else {
      Array()
    }

  }

  def main(args: Array[String]): Unit = {
    val a = Array(Array(0,2),Array(5,10),Array(13,23),Array(24,25))
    val b = Array(Array(1,5),Array(8,12),Array(15,24),Array(25,26))

    //val a1 = Array[Array[Int]](Array(5,10))
    //val b1 = Array[Array[Int]](Array(5,6))
    val res = intervalIntersection(a,b)
    for (r <- res) {
      println(r.mkString(","))
    }
  }
}