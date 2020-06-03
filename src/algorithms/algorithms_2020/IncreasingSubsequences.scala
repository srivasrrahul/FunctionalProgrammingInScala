import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def findSubsequences(nums: Array[Int]): List[List[Int]] = {

    if (nums.length >= 1) {
      val subsequences = new Array[List[List[Int]]](nums.length)
      subsequences(0) = List(List(nums(0)))

      for (j <- 1 to nums.length - 1) {
        var maxLength = 1
        var maxLst = new ListBuffer[List[Int]]
        maxLst.append(List(nums(j)))
        for (k <- j - 1 to 0 by -1) {


          if (nums(k) <= nums(j)) {
            for (prevLst <- subsequences(k)) {
              maxLst.append(prevLst ++ List(nums(j)))
            }
          }
        }


        subsequences(j) = maxLst.toList
      }

      //println(subsequences.mkString("\n"))

      var retValue = new mutable.HashSet[List[Int]]
      for (j <- 0 to subsequences.length - 1) {
        for (subsequence <- subsequences(j)) {
          if (subsequence.length >= 2) {
            retValue.add(subsequence)
          }
        }
      }

      retValue.toList

    }else {
      List()
    }

  }

  def main(args: Array[String]): Unit = {
    println(findSubsequences(Array(4, 6,7,7)))
  }


}