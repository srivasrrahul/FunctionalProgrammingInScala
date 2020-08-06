import scala.collection.mutable
case class Tuple(val startIndex : Int,val endIndex : Int)
object Solution {
  def countSubstrings(s: String): Int = {
    val matrix = Array.ofDim[Boolean](s.length,s.length)
    for (j <- 0 to s.length-1) {
      matrix(j)(j) = true
    }

    var j = 0
    var c = 1
    var k = c

    var countPalindromicSubString = s.length //single char
    while (j < s.length && k < s.length) {
      if (s(j) != s(k)) {
        matrix(j)(k) = false
      }else {
        if (j+1 == k) {
          matrix(j)(k) = true
          countPalindromicSubString = countPalindromicSubString + 1
        }else {
          matrix(j)(k) = matrix(j + 1)(k - 1)
          if (matrix(j)(k) == true) {
            countPalindromicSubString = countPalindromicSubString + 1
          }
        }
      }

      j = j + 1
      k = k + 1

      if (k == s.length) {
        c = c + 1
        if (c >= s.length) {
          //Do nothing
        }else {
          j = 0
          k = c
        }
      }
    }

    // for (j <- 0 to s.length-1) {
    //   println(matrix(j).mkString(","))
    // }

    countPalindromicSubString
  }
}