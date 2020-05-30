import scala.collection.mutable

object Solution {
  def findDuplicates(nums: Array[Int]): List[Int] = {
    val countInt = new mutable.TreeMap[Int,Set[Int]]()
    val intCount = new mutable.HashMap[Int,Int]()

    for (num <- nums) {
      val defaultCount = intCount.getOrElseUpdate(num,0)
      intCount += ((num,defaultCount+1))
    }

    for ((value,valueCount) <- intCount) {
      val defaultList = countInt.getOrElseUpdate(valueCount,Set[Int]())
      countInt += ((valueCount,defaultList.+(value)))
    }

    if (countInt.size >= 1) {
      val (lastSize,lastSet) = countInt.last
      if (lastSize == 2) {
        countInt.last._2.toList
      }else {
        List()
      }
    }else {
      List()
    }
  }

  def main(args: Array[String]): Unit = {
    println(findDuplicates(Array(4,3,2,7,8,2,3,1)))
  }
}