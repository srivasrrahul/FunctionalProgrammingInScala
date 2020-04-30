import scala.collection.mutable
import scala.util.control.Breaks._

object Solution {
  def maximumSwap(num: Int): Int = {
    val str = num.toString
    val indexMap = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()(Ordering[Int].reverse)
    var maxId = str.length
    for (j <- 0 to str.length-1) {
      val currentDigit = str(j).asDigit
      indexMap.getOrElseUpdate(currentDigit,new mutable.TreeSet[Int]()(Ordering[Int].reverse))
      indexMap.get(currentDigit).get.addOne(j)
    }

    //println(indexMap.mkString(","))

    var exchTuple = (-1,-1)
    breakable {
      for (j <- 0 to str.length - 1) {
        //println("For j " + j + " " + indexMap)
        val currentDigit = str(j).asDigit
        if (indexMap.head._1 == currentDigit) {
          //indexMap.head._2.remove(indexMap.head._2.head)
          //val rangeTo = indexMap.head._2.rangeTo(j)
          indexMap.head._2.filterInPlace(x => x > j)
          if (indexMap.head._2.size == 0) {
            indexMap.remove(indexMap.head._1)
          }

          //println("Ignoring j " + j)
          //maxStringBuffer.append(currentDigit)
        } else {
          //Repalce with largest's smallest index
          val largestSmallestIndex = indexMap.head._2.head
          exchTuple = (largestSmallestIndex,j)
          //println("Exch Tuple " + exchTuple)
          break
        }
      }
    }

    if (exchTuple._1 != -1) {

      val maxStringBuffer = new StringBuilder

      for (j <- 0 to str.length-1) {
        maxStringBuffer.append(str(j))
      }

      val t = maxStringBuffer.charAt(exchTuple._1)
      maxStringBuffer.setCharAt(exchTuple._1,maxStringBuffer.charAt(exchTuple._2))
      maxStringBuffer.setCharAt(exchTuple._2,t)

      Integer.parseInt(maxStringBuffer.toString())
    }else {
      num
    }
  }

  def main(args: Array[String]): Unit = {
    println(maximumSwap(9973))

  }
}