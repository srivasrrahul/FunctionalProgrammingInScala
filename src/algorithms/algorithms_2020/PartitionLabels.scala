import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Interval(val first : Int,val last : Int)

object Solution {
  def partitionLabels(string: String): List[Int] = {
    val intervalMap = new mutable.HashMap[Char,Interval]()
    for (j <- 0 to string.length-1) {
      intervalMap.get(string(j)) match {
        case None => {
          intervalMap += ((string(j),new Interval(j,j)))
        }
        case Some(existingInterval) => {
          intervalMap += ((string(j),new Interval(existingInterval.first,j)))
        }
      }
    }

    val intervalArr : Array[Interval] = intervalMap.values.toArray
    scala.util.Sorting.quickSort(intervalArr)(new Ordering[Interval] {
      override def compare(x: Interval, y: Interval): Int = {
        x.first.compareTo(y.first)
      }
    })

    //println(intervalArr.mkString(","))


    val mergedIntervals = new ListBuffer[Interval]
    mergedIntervals.addOne(intervalArr(0))

    def merge(i1 : Interval,i2 : Interval) : List[Interval] = {
      //println("y")
      if (i2.first < i1.last) {
        //println("z")
        val maxIndex = if (i2.last > i1.last) i2.last else i1.last
        List(new Interval(i1.first,maxIndex))
      }else {
        //println("Here")
        List(i1,i2)
      }
    }
    for (j <- 1 to intervalArr.length-1) {
      val current = intervalArr(j)
      val mergedInterval = merge(mergedIntervals.last,current)
      mergedIntervals.dropRightInPlace(1)
      mergedIntervals.addAll(mergedInterval)
    }

    //println(mergedIntervals.mkString(","))
    val retValue = new ListBuffer[Int]
    for (interval <- mergedIntervals) {
      val sizeInterval = interval.last - interval.first+ 1
      retValue.append(sizeInterval)
    }

    retValue.toList


  }

  def main(args: Array[String]): Unit = {

    println(partitionLabels("ababcbacadefegdehijhklij"))
  }
}