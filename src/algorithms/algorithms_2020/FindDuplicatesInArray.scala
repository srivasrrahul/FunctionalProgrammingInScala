import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {

  def findDuplicatesLinear(nums: Array[Int]) : List[Int] = {
    val countArr = new Array[Int](nums.length+1) //0 is not used 1..N

    var retValue = new ListBuffer[Int]
    for (num <- nums) {
      countArr(num) = countArr(num) + 1
      if (countArr(num) == 2) {
        retValue.append(num)
      }
    }

    retValue.toList
  }

  def findDuplicates(nums: Array[Int]): List[Int] = {
    findDuplicatesLinear(nums)
//    val countInt = new mutable.TreeMap[Int,Set[Int]]()
//    val intCount = new mutable.HashMap[Int,Int]()
//
//    for (num <- nums) {
//      val defaultCount = intCount.getOrElseUpdate(num,0)
//      intCount += ((num,defaultCount+1))
//    }
//
//    for ((value,valueCount) <- intCount) {
//      val defaultList = countInt.getOrElseUpdate(valueCount,Set[Int]())
//      countInt += ((valueCount,defaultList.+(value)))
//    }
//
//    if (countInt.size >= 1) {
//      val (lastSize,lastSet) = countInt.last
//      if (lastSize == 2) {
//        countInt.last._2.toList
//      }else {
//        List()
//      }
//    }else {
//      List()
//    }
  }



  def main(args: Array[String]): Unit = {
    println(findDuplicates(Array(4,3,2,7,8,2,3,1)))
  }
}