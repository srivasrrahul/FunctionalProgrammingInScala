import scala.util.control.Breaks._

object Solution {
  def isPalindrome(s: String): Boolean = {
    if (s.length == 0) {
      true
    }else {
      var left = 0
      var right = s.length-1

      val stringBuilder = new StringBuilder
      for (j <- 0 to s.length-1) {
        s.charAt(j) match {
          case x if Character.isAlphabetic(s.charAt(j)) || Character.isDigit(s.charAt(j)) => {
            stringBuilder.append(s.charAt(j).toLower)
          }
          case _ => {

          }
        }
      }

      //println(stringBuilder.toString())
      var palindrome = true
      breakable {
        for (j <- 0 to (stringBuilder.length() / 2)-1) {
          if (stringBuilder.charAt(j) != stringBuilder.charAt(stringBuilder.length()-1-j)) {
            palindrome = false

            break
          }
        }
      }

      palindrome
    }
  }

  def main(args: Array[String]): Unit = {
    println(isPalindrome("a b a "))
  }
}