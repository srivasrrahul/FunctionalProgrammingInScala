import scala.collection.mutable

object Solution {
  def subarraySum(nums: Array[Int], k: Int): Int = {
    val arrSum = new Array[Int](nums.length)
    arrSum(0) = nums(0)
    for (j <- 1 to nums.length-1) {
      arrSum(j) = arrSum(j-1) + nums(j)
    }

    def findTwoSums(arr : Array[Int],totalSum : Int) : Int = {
      //println(arr.mkString(",") + " " + totalSum)
      var totalCount = 0
      val sumMap = new mutable.HashMap[Int,Int]()
      for (j <- 0 to arr.length-1) {
        val pendingSum = arr(j) - totalSum
        //println("For j = " + j + " pendingSum = " + pendingSum + " " + sumMap)

        if (pendingSum == 0) {
          //Implies from 0ths index to current
          //println("here")
          totalCount = totalCount + 1
          if (arr(j) == 0) {
            val defaultCount = sumMap.getOrElse(pendingSum, 0)
            totalCount = totalCount + defaultCount
          }
        }else {
          val defaultCount = sumMap.getOrElse(pendingSum, 0)
          totalCount = totalCount + defaultCount
        }

        val defaultCurrentCount = sumMap.getOrElse(arr(j),0)
        sumMap += ((arr(j),defaultCurrentCount+1))


      }


      totalCount


    }

    findTwoSums(arrSum,k)
  }

  def main(args: Array[String]): Unit = {

    println(subarraySum(Array(0,0),0))
  }
}