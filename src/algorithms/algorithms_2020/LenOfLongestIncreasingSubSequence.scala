import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Solution {
  def findNumberOfLIS(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      val countLIS = new Array[(Int,Int)](nums.length) //len and its count
      countLIS(0) = (1,1)

      for (j <- 1 to nums.length-1) {
        val countMap = new mutable.TreeMap[Int,Int]()(Ordering[Int].reverse)
        for (k <- j-1 to 0 by -1) {
          if (nums(j) > nums(k)) {
            val localCountLIS = countLIS(k)._1 + 1
            val defaultCount = countMap.getOrElseUpdate(localCountLIS,0)
            countMap += ((localCountLIS,defaultCount+countLIS(k)._2))
          }
        }

        //println("For j " + j + " " + countMap)

        val localCountOnlySelf = 1
        val defaultCount = countMap.getOrElseUpdate(localCountOnlySelf,0)
        countMap += ((localCountOnlySelf,defaultCount+1))

        countLIS(j) = (countMap.head._1,countMap.head._2)
      }

      //println(countLIS.mkString(","))
      val maxCount = countLIS.max(new Ordering[(Int,Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          x._1.compareTo(y._1)
        }
      })._1

      //println(maxCount)
      var totalCount = 0
      for ((len,count) <- countLIS) {
        if (len >= maxCount) {
          totalCount = totalCount + count
        }
      }

      totalCount


      //0

    }else {
      0
    }
  }



  //1,2,4,5,7
  //1,2,3,4,7
  //1,2,3,5,7
  def main(args: Array[String]): Unit = {
    println(findNumberOfLIS(Array(1,3,5,4,7)))
    println(findNumberOfLIS(Array(2,2,2,2,2)))
    println(findNumberOfLIS(Array(1,2,4,3,5,4,7,2)))
  }

}