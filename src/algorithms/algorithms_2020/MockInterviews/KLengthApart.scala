import scala.collection.mutable

object Solution {
  def kLengthApart(nums: Array[Int], k: Int): Boolean = {
    val oneIndex = new mutable.ArrayBuffer[Int]()

    var valid = true
    for (j <- 0 to nums.length-1 if valid == true) {
      val v = nums(j)
      if (oneIndex.size > 0 && v == 1) {
        val last = oneIndex.last
        if (math.abs(j-last-1) < k) {
          //println(" false " + j + " " + oneIndex)
          valid = false
        }
      }

      if (v == 1) {
        oneIndex.append(j)
      }
    }
    println(oneIndex)

    valid
  }
}