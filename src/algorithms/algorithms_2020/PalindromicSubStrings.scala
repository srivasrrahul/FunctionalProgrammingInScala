import scala.collection.mutable
case class Tuple(val startIndex : Int,val endIndex : Int)
object Solution {
  def countSubstrings(s: String): Int = {
    val cache = new mutable.HashMap[Tuple,Boolean]()
    def isPalindrom(j : Int,k : Int) : Boolean = {
      if (j == k) {
        true
      }else {
        val key = new Tuple(j,k)
        if (cache.contains(key)) {
          cache.get(key).get
        }else {
          if (s(j) == s(k)) {
            if (j + 1 < k) {
              val retValue = isPalindrom(j + 1, k - 1)
              cache += ((key,retValue))
              retValue
            } else {
              true
            }
          } else {
            false
          }
        }
      }
    }

    var countPalindromicSubString = 0
    for (j <- 0 to s.length-1) {
      for (k <- j to s.length-1) {
        if (isPalindrom(j,k)) {
          countPalindromicSubString = countPalindromicSubString + 1
        }
      }
    }

    countPalindromicSubString
  }
}