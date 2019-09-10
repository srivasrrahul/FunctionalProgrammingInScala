import scala.collection.mutable.HashMap

object Solution {
  def lengthOfLongestSubstring(s: String): Int = {
    if (s.isEmpty) {
      0
    }else {
      var index = new HashMap[Char, Int]
      index += s.charAt(0) -> 0
      var maxLength = 1
      for (j <- 1 to s.length - 1) {
        //println(index)
        val current = s.charAt(j)
        val pastIndex = index.get(current)
        pastIndex match {
          case Some(pastIndexVal) => {
            //index.dropWhile({case (k,v) => v > pastIndexVal})
            index = index.filter({ case (k, v) => v > pastIndexVal })
            index += (current -> j)
          }
          case None => {
            index += (current -> j)
          }
        }

        if (index.contains(current)) {
          val pastIndex = index.get(current)
        } else {
          index += (current -> j)
        }

        if (index.size > maxLength) {
          maxLength = index.size
        }
      }

      //println(index)

      maxLength
    }
  }

//  def main(args: Array[String]): Unit = {
//    println(lengthOfLongestSubstring("abcabcbb"))
//  }
}