object Solution {
  def repeatedSubstringPattern(s: String): Boolean = {
    //abcdabcd
    var ifPossible = false
    for (j <- 0 to s.length-1 if ifPossible == false) {
      if (j != s.length-1) {
        val begin = 0
        val end = j
        val len = end-begin+1
        if (s.length % len == 0) {
          val maxTimes = (s.length / len)
          var times = 1

          //println("here " + begin + " " + end + " " + maxTimes)
          var matchFound = true
          while (times < maxTimes && matchFound == true) {
            //println(times)
            var initIndex = times*len
            for (j <- 0 to len-1 if matchFound == true) {
              if (s(initIndex+j) != s(begin + j)) {
                matchFound = false
              }
            }

            times = times + 1
          }


          if (matchFound == true) {
            //Found
            ifPossible = true
          }

        }


      }
    }

    ifPossible
  }

  def main(args: Array[String]): Unit = {
    println(repeatedSubstringPattern("abcabcabcabc"))
  }
}