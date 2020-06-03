import scala.collection.mutable

object Solution {




  def increasingTriplet(nums: Array[Int]): Boolean = {

    if (nums.length > 0) {
      //increasingTripletOptimal(nums)
      val longestSubSeqeuence = new Array[Int](nums.length)
      longestSubSeqeuence(0) = 1

      var ifFound = false
      for (j <- 1 to nums.length - 1 if ifFound == false) {
        var maxLength = 1
        for (k <- j - 1 to 0 by -1) {
          if (nums(k) < nums(j)) {
            val currentLength = longestSubSeqeuence(k) + 1
            if (currentLength > maxLength) {
              maxLength = currentLength
            }
          }
        }

        longestSubSeqeuence(j) = maxLength
        if (maxLength >= 3) {
          ifFound = true
        }
      }

      //println(longestSubSeqeuence.mkString(","))
      //longestSubSeqeuence.filter(len => len >= 3).isEmpty == false
      ifFound
    }else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    println(increasingTriplet(Array(1,0,0,0,10,0,0,0,1000)))
  }
}