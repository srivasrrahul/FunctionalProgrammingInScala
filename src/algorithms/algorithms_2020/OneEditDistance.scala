object Solution {
  def isOneEditDistance(s: String, t: String): Boolean = {
    //Plain edit distance
    val diffLen = scala.math.abs(s.length - t.length)

    if (diffLen > 1) {
      false
    }else {
      var possible = false
      val tSet = t.toSet
      if (s.length < t.length) {
        //add a char from tSet at different location
        for (tChar <- tSet if possible == false) {
          for (j <- 0 to s.length if possible == false) {
            val prefix = s.substring(0,j)
            val newStr = tChar.toString
            val suffix = s.substring(j)
            //println(prefix + " " + newStr + " " + suffix)
            if ((prefix ++ newStr ++ suffix) == t) {
              possible = true
            }
          }
        }

        if (possible == false) {
          if (s.length == 0) {
            possible = true
          }
        }
      }



      if (possible == false) {
        if (s.length > t.length) {
          //delete one char from s
          //println("here")

          for (j <- 0 to s.length-1 if possible == false) {
            val stringBuilder = new StringBuilder(s)
            //println(stringBuilder.deleteCharAt(j).toString())
            if (stringBuilder.deleteCharAt(j).toString() == t) {
              possible = true
            }
          }

          if (possible == false) {
            if (t.length == 0) {
              possible = true
            }
          }
        }


      }

      if (possible == false) {
        if (s.length == t.length && s != t) {

          //println("here")
          var diffCount = 0
          for (j <- 0 to s.length-1 if possible == false) {
            if (s(j) != t(j)) {
              diffCount = diffCount + 1
            }
//            for (tChar <- tSet if possible == false) {
//              val stringBuilder = new StringBuilder(s)
//              if (stringBuilder.setCharAt(j,tChar).toString() == t) {
//                possible = true
//              }
//            }

          }
//
          if (diffCount == 1) {
            possible = true
          }

        }
      }

      possible


    }
  }



  def main(args: Array[String]): Unit = {
    println(isOneEditDistance("ab","ac"))
  }
}