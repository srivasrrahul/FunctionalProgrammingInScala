import scala.collection.mutable.ListBuffer

object Solution {
  def findAnagrams(s: String, p: String): List[Int] = {
    val pLength = p.length
    val sortedP = p.toSeq.sorted.unwrap
    var j = 0
    val retValue = new ListBuffer[Int]
    while (j < s.length && j + pLength <= s.length) {
      val subStr = s.substring(j,j+pLength)
      if (subStr.length == sortedP.length) {
        val sortedSubStr = subStr.toSeq.sorted.unwrap
        if (sortedSubStr == sortedP) {
          retValue.addOne(j)
        }
      }

      j = j + 1
    }

    retValue.toList
  }

  def main(args: Array[String]): Unit = {
    println(findAnagrams("",""))
  }
}