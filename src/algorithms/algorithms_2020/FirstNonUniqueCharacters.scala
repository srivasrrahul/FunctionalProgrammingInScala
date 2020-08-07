import scala.collection.mutable

object Solution {
  def firstUniqChar(s: String): Int = {
    val charCount = new mutable.HashMap[Char,(Int,Int)]()
    val countChar = new mutable.TreeSet[Int]() //count and its index

    for (j <- 0 to s.length-1) {
      val defaultValue = charCount.getOrElseUpdate(s(j),(-1,0)) //default has first index as -1
      val newIndex = if (defaultValue._1 == -1) j else defaultValue._1
      charCount += ((s(j),(newIndex,defaultValue._2+1)))
    }

    for ((ch,(firstIndex,count)) <- charCount) {
      if (count == 1) {
        countChar.add(firstIndex)
      }
    }
    if (countChar.isEmpty == true) {
      -1
    }else {
      countChar.head
    }

  }
}