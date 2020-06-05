import java.util

import scala.collection.mutable

object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val presence = new mutable.HashMap[Int,Int]()
    var ifPresent = false
    var j = 0
    val result = new Array[Int](2)
    for (num <- nums if ifPresent == false) {
      val diff = target - num
      //println(num + " " + diff)
      if (presence.contains(diff)) {
        ifPresent = true
        result(0) = j
        result(1) = presence.get(diff).get
      }else {
        presence += ((num,j))
      }

      j = j + 1
    }

    result
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(2, 7, 11, 15)
    println(twoSum(arr,13).mkString(","))
  }
}