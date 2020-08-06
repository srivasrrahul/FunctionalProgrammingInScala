
object Solution {
  def longestDupSubstring(S: String): String = {
    val arr = new Array[Int](S.length)
    for (j <- 0 to arr.length-1) {
      arr(j) = j
    }

    arr.sortInPlace()(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = {
        var comp = S(x).compareTo(S(y))
        var j = x
        var k = y
        while (comp == 0 && j < S.length && k < S.length) {
          comp = S(j).compareTo(S(k))
          j = j + 1
          k = k + 1
        }

        if (comp == 0) {
          y.compareTo(x)
        }else {
          comp
        }
      }
    })
    //abc
    //abc,bc,c

    def longestCommonPrefix(j : Int, k : Int) : Int = {
      var x = j
      var y = k
      var count = 0
      var equal = true
      while (x < S.length && y < S.length && equal == true) {
        if (S(x) == S(y)) {
          count = count + 1
          x = x + 1
          y = y + 1
        }else {
          equal = false
        }
      }

      count
    }
    var longestCommonSubStringIndex = (0,0)
    for (j <- 0 to arr.length-2) {
      val len = longestCommonPrefix(arr(j),arr(j+1))
      if (len > (longestCommonSubStringIndex._2-longestCommonSubStringIndex._1)) {
        longestCommonSubStringIndex = (arr(j),arr(j)+len)
      }
    }

    if (longestCommonSubStringIndex._2 > longestCommonSubStringIndex._1) {
      S.substring(longestCommonSubStringIndex._1,longestCommonSubStringIndex._2)
    }else {
      ""
    }
  }
}