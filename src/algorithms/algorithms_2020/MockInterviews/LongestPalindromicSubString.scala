import scala.collection.mutable

object Solution {
  def longestPalindrome(s: String): String = {
    val map = new mutable.HashMap[Char,mutable.ArrayBuffer[Int]]()
    for (j <- 0 to s.length-1) {
      val defSet = map.getOrElseUpdate(s(j),new mutable.ArrayBuffer[Int]())
      defSet.append(j)
    }

    //println(map)
    def isPalindrome(j : Int, k : Int) : Boolean = {
      if (k == j) {
        true
      }else {
        if (k == j+1) {
          s(j) == s(k)
        }else {
          s(j) == s(k) && isPalindrome(j+1,k-1)
        }
      }

    }

    var maxSize = 1
    var maxString = (0,0)
    for ((ch,set) <- map) {
      if (set.size > 1) {
        //println("For " + ch)
        for (j <- 0 to set.length-1) {
          var found = false

          for (k <- set.length-1 to j+1 by -1 if found == false) {
            //println(s.substring(j,k+1))
            if (isPalindrome(set(j),set(k))) {
              //println(j + " " + k)
              found = true
              if ((set(k)-set(j)+1) > maxSize) {
                maxSize = set(k)-set(j)+1
                maxString = (set(j),set(k))
              }
            }
          }
        }
      }
    }

    s.substring(maxString._1,maxString._2+1)

  }

  def main(args: Array[String]): Unit = {
    println(longestPalindrome("xaabacxcabaaxcabaax"))
  }
}