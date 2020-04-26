import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks._
object Solution {

  def validPalindrome(s: String): Boolean = {
    def checkPalidomr(string: String,beginIndex : Int,endIndex : Int) : Boolean = {
      var isValidPalindrome = true
      breakable {
        var start  = beginIndex
        var end = endIndex

        while (start <= end) {
          if (string.charAt(start) != string.charAt(end)) {
            isValidPalindrome = false
            break
          }else {
            start = start + 1
            end = end-1
          }
        }
      }

      isValidPalindrome
    }

    var retValue = true
    if (retValue == true) {
      var begin = 0
      var end = s.length-1

      var count = 0
      breakable {
        while (begin <= end) {
          if (s.charAt(begin) == s.charAt(end)) {
            begin = begin + 1
            end = end - 1
          } else {
            if (count > 0) {
              break
            } else {
              //ignore left
              val leftIgnore = checkPalidomr(s, begin + 1, end)
              if (leftIgnore == true) {
                retValue = true
                break
              } else {
                val rightIgnore = checkPalidomr(s, begin, end - 1)
                if (rightIgnore == true) {
                  retValue = true
                  break
                } else {
                  retValue = false
                  break
                }
              }
            }
          }
        }
      }
    }

    retValue
  }

  def main(args: Array[String]): Unit = {

    println(validPalindrome("abcdcbea"))
  }
}