import scala.collection.mutable

object Solution {
  def longestPalindrome(s: String): Int = {
    //abccccdd
    val countChar = new mutable.TreeMap[Int,mutable.HashSet[Char]]()(Ordering[Int].reverse)
    val charCount = new mutable.HashMap[Char,Int]()
    for (ch <- s) {
      val defaultCount = charCount.getOrElseUpdate(ch,0)
      charCount += ((ch,defaultCount+1))
    }

    for ((ch,count) <- charCount) {
      val set = countChar.getOrElseUpdate(count,new mutable.HashSet[Char]())
      set.add(ch)
    }

    //println(countChar)
    var maxCount = 0
    var anyFurtherAdditionPossible = true
    var tempSize = 0
    while (countChar.isEmpty == false && anyFurtherAdditionPossible == true) {
      //remove head
      val (headCount,headSet) = countChar.head
      //println(headCount + " " + headSet)
      countChar.remove(headCount)

      if (headCount > 1) {
        var maxCountPossibleInCurrent = headCount / 2
        maxCount = maxCount + (maxCountPossibleInCurrent * headSet.size) * 2

        if (headCount % 2 == 0) {

        }else {
          tempSize = 1
        }
      }else {
        maxCount += 1
        tempSize = 0
        anyFurtherAdditionPossible = false
      }
    }

    maxCount + tempSize


  }

  def main(args: Array[String]): Unit = {
    println(longestPalindrome("abccccdd"))

  }
}