import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._
object Solution {
  def longestPalindrome(s: String): String = {
    def getLongestCommon(s1 : String,s2 : String) : String = {
      //println("S1 = " + s1 + " S2 = " + s2)
      val strBuilder = new StringBuilder
      val maxLength : Int = if (s1.size > s2.size) s2.size else s1.size
      breakable {
        for (j <- 0 to maxLength-1) {
          if (s1.charAt(j) == s2.charAt(j)) {
            strBuilder.append(s1.charAt(j))
          }else {
            break
          }
        }
      }

      strBuilder.toString
    }

    def isPalindrome(s : String) : Boolean = {
      s.equals(s.reverse)
    }
    val l = s.length
    val suffixArray = new Array[String](2*l)
    for (i <- 0 to s.length-1) {
      val substr = s.substring(i)
      suffixArray(i) = substr
    }

    val r = s.reverse
    for (i <- 0 to r.length-1) {
      val substr = r.substring(i)
      suffixArray(r.length + i) = substr
    }

    //println(suffixArray.mkString(","))
    scala.util.Sorting.quickSort(suffixArray)
    var longestPalindromicSubString = ""
    //println(suffixArray.size)
    //println(suffixArray.mkString(","))
    for (j <- 0 to suffixArray.size-2) {
      //println("Current index " + j)
      val curren = suffixArray(j)
      val next = suffixArray(j+1)
      val longestCommon = getLongestCommon(curren,next)
      if (longestCommon.length > longestPalindromicSubString.length && isPalindrome(longestCommon)) {
        longestPalindromicSubString = longestCommon
      }
    }

    longestPalindromicSubString
  }

//  def main(args: Array[String]): Unit = {
//    println(longestPalindrome("wehadastrictmadaminschool"))
//  }
}