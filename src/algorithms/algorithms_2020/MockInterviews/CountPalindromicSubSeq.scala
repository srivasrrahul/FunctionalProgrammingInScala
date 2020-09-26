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

    def commonSubSeq(x1 : Int,x2 : Int,y1 : Int,y2 : Int) : Set[String] = {
      //println(x1 + " " + y1 + " " + x2 + " " + y2)
      if (x1 == x2) {
        if (s2.substring(y1,y2+1).contains(s1(x1))) {
          Set(s1(x1).toString)
        }else {
          Set()
        }
      }else {
        if (y1 == y2) {
          if (s1.substring(x1,x2+1).contains(s2(y1))) {
            Set(s2(y1).toString)
          }else {
            Set()
          }
        }else {
          if (s1(x2) == s2(y2)) {
            val lst = commonSubSeq(x1,x2-1,y1,y2-1)
            val retValue = new mutable.HashSet[String]
            for (l <- lst) {
              retValue.add(l ++ s1(x2).toString)
            }

            retValue.add(s1(x2).toString)
            retValue.addAll(lst).toSet
          }else {
            val l1 = commonSubSeq(x1,x2-1,y1,y2)
            val l2 = commonSubSeq(x1,x2,y1,y2-1)

            l1 ++ l2


          }
        }

      }
    }

    val common = commonSubSeq(0,s1.length-1,0,s2.length-1).filter(x => isPalindrome(x))
    println(common)
    1
  }

  def main(args: Array[String]): Unit = {
    println(countPalindromicSubsequences("bccb"))
  }
}