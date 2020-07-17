import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def wordSubsets(A: Array[String], B: Array[String]): List[String] = {
    val charCounts = new ListBuffer[mutable.HashMap[Char,Int]]
    for (a <- A) {
      val charCount = new mutable.HashMap[Char,Int]()
      for (ch <- a) {
        val defaultCount = charCount.getOrElse(ch,0)
        charCount += ((ch,defaultCount+1))
      }

      charCounts.append(charCount)
    }

    val keyCounts = new mutable.HashSet[mutable.HashMap[Char,Int]]

    for (b <- B) {
      val charCount = new mutable.HashMap[Char,Int]()
      for (ch <- b) {
        val defaultCount = charCount.getOrElse(ch,0)
        charCount += ((ch,defaultCount+1))
      }

      keyCounts.add(charCount)
    }

    val retValue = new ListBuffer[String]
    var j = 0
    for (charCount <- charCounts) {

      var foundAtAllPlace = true
      for (keyCount <- keyCounts if foundAtAllPlace == true) {
        for ((key,count) <- keyCount if foundAtAllPlace == true) {

          charCount.get(key) match {
            case None => {
              foundAtAllPlace = false
            }
            case Some(charCounter) => {
              if (charCounter < count) {
                foundAtAllPlace = false
              }
            }
          }
        }
      }

      if (foundAtAllPlace) {
        retValue.append(A(j))
      }
      j = j + 1
    }


    retValue.toList

  }

  def main(args: Array[String]): Unit = {
    println(wordSubsets(Array("amazon","apple","facebook","google","leetcode"),Array("e","o")))
  }


}