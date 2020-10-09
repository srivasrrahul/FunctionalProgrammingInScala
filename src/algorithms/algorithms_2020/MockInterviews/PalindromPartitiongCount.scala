import scala.collection.mutable
import scala.util.hashing.Hashing

case class Index(val x : Int,val y : Int)
object Solution {
  def minCut(s: String): Int = {
    def isPalindrome(j : Int,k : Int) : Boolean = {
      if (j == k) {
        true
      }else {
        if (s(j) == s(k)) {
          if (j+1 == k) {
            true
          }else {
            isPalindrome(j+1,k-1)
          }
        }else {
          false
        }
      }
    }

    val cache = new mutable.HashMap[Index,Int]() {}
    def itr(j : Int,k : Int) : Int = {
      if (j == k) {
        0
      }else {
        if (isPalindrome(j,k)) {
          0
        }else {
          val index = new Index(j,k)
          if (cache.contains(index)) {
            cache.get(index).get
          }else {
            var minCuts = Int.MaxValue
            for (p <- j to k - 1) {
              if (isPalindrome(j, p)) {
                val cutNeeded = itr(p + 1, k)
                if (cutNeeded + 1 < minCuts) {
                  minCuts = cutNeeded + 1
                }
              }
            }

            cache += ((index,minCuts))
            minCuts
          }
        }
      }
    }

    itr(0,s.length-1)
  }

  def main(args: Array[String]): Unit = {
    println(minCut("aab"))
  }
}