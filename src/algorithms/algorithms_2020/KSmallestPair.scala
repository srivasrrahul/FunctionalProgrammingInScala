import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Solution {
  def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], K: Int): List[List[Int]] = {
    val sortedArrBuffer = new ListBuffer[List[Int]]()
    var j = 0
    var k = 0
    while (j < nums1.length && k < nums2.length && sortedArrBuffer.length < K) {
      sortedArrBuffer.append(List(nums1(j),nums(k)))
      var option1 = Int.MaxValue
      if (j+1 < nums1.length) {
        option1 = nums1(j+1) + nums2(k)
      }

      var option2 = Int.MaxValue
      if (k+1 < nums2.length) {
        option2 = nums1(j) + nums2(k+1)
      }

      if (option1 <= option2) {
        j = j+1
      }else {
        k = k+1
      }
    }

    if (sortedArrBuffer.length < K) {
      while (j < nums1.length && sortedArrBuffer.length < K) {
        sortedArrBuffer.append(List(nums1,nums2.last))
        j = j + 1
      }

      while (k < nums2.length && sortedArrBuffer.length < K) {
        sortedArrBuffer.append(List(nums1.last,nums2(k)))
        k = k + 1
      }

    }

    sortedArrBuffer.toList
  }

  def main(args: Array[String]): Unit = {
    println(kSmallestPairs(Array(1,7,11),Array(2,4,6),3))
  }
}