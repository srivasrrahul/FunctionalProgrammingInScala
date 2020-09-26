import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def countPalindromicSubsequences(S: String): Int = {
    val s1 = S
    val s2 = S.reverse
    def isPalindrome(s : String) : Boolean = {
      def itr(x : Int,y : Int) : Boolean = {
        if (x == y) {
          true
        }else {
          if (x+1 == y) {
            s(x) == s(y)
          }else {
            s(x) == s(y) && itr(x+1,y-1)
          }
        }
      }

      itr(0,s.length-1)
    }

    val arr = Array.ofDim[Set[String]](s1.length,s2.length)
    var matched : Set[String] = Set()
    for (j <- 0 to arr.length-1) {
      if (s1(j) == s2(0)) {
        matched = Set(s2(0).toString)
      }

      arr(j)(0) = matched
    }

    matched = Set()

    for (k <- 0 to arr(0).length-1) {
      if (s2(k) == s1(0)) {
        matched = Set(s1(0).toString)
      }

      arr(0)(k) = matched
    }

    for (j <- 1 to arr.length-1) {
      for (k <- 1 to arr(j).length-1) {
        if (s1(j) == s2(k)) {
          val prev = arr(j-1)(k-1)
          val set = new mutable.HashSet[String]()
          for (p <- prev) {
            set.add(p ++ s1(j).toString)
          }

          set.addAll(prev)
          set.add(s1(j).toString)
          arr(j)(k) = set.toSet
        }else {
          arr(j)(k) = arr(j-1)(k) ++ arr(j)(k-1)
        }
      }
    }
    arr(s1.length-1)(s2.length-1).filter(x => isPalindrome(x)).size
  }

  def main(args: Array[String]): Unit = {
    println(countPalindromicSubsequences("bccb"))
  }
}