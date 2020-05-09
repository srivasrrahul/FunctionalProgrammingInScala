import scala.collection.mutable

object Solution {
  def frequencySort(s: String): String = {
    val charCount = new mutable.HashMap[Char,Int]()
    val countChar = new mutable.TreeMap[Int,List[Char]]()(Ordering[Int].reverse)

    for (ch <- s) {
      val currentCount = charCount.getOrElseUpdate(ch,0)
      charCount += ((ch,currentCount+1))
    }

    charCount.foreachEntry((ch,count) => {
      val currentCharList = countChar.getOrElseUpdate(count,List())
      countChar += ((count,ch :: currentCharList))
    })

    val sortedStr = new StringBuilder
    countChar.foreachEntry((count,chLst) => {
      for (ch <- chLst) {
        sortedStr.append(ch.toString * charCount.get(ch).get)
      }
    })

    sortedStr.toString()
  }

  def main(args: Array[String]): Unit = {
    println(frequencySort("Aabb"))
  }
}