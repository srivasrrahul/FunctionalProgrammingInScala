import scala.collection.mutable
import scala.util.control.Breaks._
object Solution {
  def rotateString(A: String, B: String): Boolean = {
    if (A.length != B.length) {
      false
    } else {
      if (B == "") {
        A == ""
      } else {
        val firstChar = A.charAt(0)
        val matchedIndex = new mutable.TreeSet[Int]()

        for (k <- 0 to B.length - 1) {
          if (B.charAt(k) == firstChar) {
            matchedIndex.add(k)
          }
        }

        var globalCheck = false
        breakable {
          for (index <- matchedIndex) {
            var k = index
            var result = true //optimistic
            breakable {
              for (j <- 0 to A.length - 1) {
                if (A.charAt(j) == B.charAt(k)) {
                  k = (k + 1) % B.length //check 0
                } else {
                  result = false
                  break
                }
              }
            }
            if (result == true) {
              //yeah found
              globalCheck = true
              break
            }
          }
        }

        globalCheck
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(rotateString("abcde","abced"))
  }

}