import scala.collection.mutable

object Solution {
  def lengthOfLongestSubstringTwoDistinct(s: String): Int = {
    var countCurrentDistinct = 0
    var countMaxDistinct = 0

    val distinctChars = new mutable.HashSet[Char]()
    var windowBegin = 0
    for (j <- 0 to s.length-1) {
      val ch = s.charAt(j)
      if (distinctChars.contains(ch)) {
        countCurrentDistinct = countCurrentDistinct + 1
        countMaxDistinct = math.max(countCurrentDistinct,countMaxDistinct)
      }else {
        if (distinctChars.size < 2) {
          distinctChars.add(ch)
          countCurrentDistinct = countCurrentDistinct + 1
          countMaxDistinct = math.max(countCurrentDistinct,countMaxDistinct)
        }else {
          //Take current and prev till its same
          countCurrentDistinct = 2
          var diff = false
          distinctChars.clear()
          distinctChars.add(ch)
          distinctChars.add(s(j-1))
          for (k <- j-2 to 0 by -1 if diff == false) {
            if (s(k) != s(j-1) && s(k) != s(j)) {
              //distinctChars.remove(s(k))
              diff = true
            }else {
              countCurrentDistinct = countCurrentDistinct + 1
              //distinctChars.remove(s(k))
              windowBegin = k
            }
          }

          //distinctChars.add(ch)
          //countCurrentDistinct = k
          countMaxDistinct = math.max(countCurrentDistinct,countMaxDistinct)

        }


      }

      //println(j + " " + s(j) + " " + countCurrentDistinct)
    }

    countMaxDistinct
  }
}