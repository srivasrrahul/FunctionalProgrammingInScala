import scala.collection.mutable

case class Point(val x : Int,val y : Int)
object Solution {
  def minAreaRect(points: Array[Array[Int]]): Int = {

    val xMap = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()
    //val yMap = new mutable.TreeMap[Int,mutable.TreeSet[Point]]()

    for (point <- points) {
      val pointElem = new Point(point(0),point(1))
      val xSet = xMap.getOrElseUpdate(point(0),new mutable.TreeSet[Int]())
      xSet.add(point(1))
      xMap += ((point(0),xSet))
    }

    //println(xMap)
    var firstItr = xMap.iterator

    var minArea = Int.MaxValue
    while (firstItr.isEmpty == false) {

      val firstValue = firstItr.next()

      //println("Insidef first value " + firstValue._1)
      val (itr1,itr) = firstItr.duplicate

      firstItr = itr1


      while (itr.isEmpty == false) {
        val secondValue = itr.next()
        //println("Second value " + secondValue._1)
        val intersection = firstValue._2.intersect(secondValue._2).toArray
        //println(intersection)
        if (intersection.size > 1) {
          for (j <- 1 to intersection.length-1) {
            val area = (intersection(j) - intersection(j-1)) * (secondValue._1-firstValue._1)
            if (area < minArea) {
              minArea = area
            }
          }
        }
      }

      //println(firstItr.isEmpty)
    }

    if (minArea == Int.MaxValue) {
      0
    }else {
      minArea
    }
  }

  def main(args: Array[String]): Unit = {
    println(minAreaRect(Array(Array(1,1),Array(1,3),Array(3,1),Array(3,3),Array(2,2))))
    println(minAreaRect(Array(Array(1,1),Array(1,3),Array(3,1),Array(3,3),Array(4,1),Array(4,3))))
    val arr = Array(Array(0,1),Array(3,2),Array(5,5),Array(4,5),Array(4,4),Array(2,0),Array(2,3),Array(2,2),
                    Array(1,0),Array(5,1),Array(2,5),Array(5,2),Array(2,4),Array(4,0))
    println(minAreaRect(arr))
  }
}