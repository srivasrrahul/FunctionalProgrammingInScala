import scala.collection.mutable

object Solution {
  def lengthOfLongestSubstring(s: String): Int = {
    val charIndex = new mutable.HashMap[Char,Int]()
    val indexChar = new mutable.TreeMap[Int,Char]()

    var maxSize = 0
    for (j <- 0 to s.length-1) {
      maxSize = math.max(maxSize,charIndex.size)
      val ch = s(j)
      if (charIndex.contains(ch) == false) {
        charIndex += ((ch,j))
        indexChar += ((j,ch))
      }else {
        val earlierIndex = charIndex.get(ch).get
        val priorLst = indexChar.rangeTo(earlierIndex)
        for ((_,oldCh) <- priorLst) {
          charIndex.remove(oldCh)
        }

        for (oldIndex <- priorLst.keys.toList) {
          indexChar.remove(oldIndex)
        }

        charIndex += ((ch,j))
        indexChar += ((j,ch))
      }
    }

    maxSize = math.max(maxSize,charIndex.size)
    maxSize
  }
}