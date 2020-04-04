import util.control.Breaks._
object Solution {
  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.length == 0) {
      ""
    }else {
      val sortedStrings = strs.sortWith(_ < _)
      //val sortedStrings = strs
      var commonLength = 0
      breakable {
        for (j <- 0 to sortedStrings(0).length - 1) {
          var currentChar = sortedStrings(0).charAt(j)
          var anomalyFound = false
          for (k <- 1 to sortedStrings.length - 1) {
            if (sortedStrings.length < j || sortedStrings(k)(j) != currentChar) {
              anomalyFound = true
              break
            }
          }

          if (anomalyFound) {
            break
          } else {
            commonLength += 1
          }
        }
      }

      //println("Common length is " + commonLength)

      sortedStrings(0).substring(0, commonLength)
    }
  }

  def main(args: Array[String]): Unit = {
    println(longestCommonPrefix(Array()))
  }
}