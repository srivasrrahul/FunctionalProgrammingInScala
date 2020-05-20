import scala.collection.mutable

object Solution {
  def lengthOfLongestSubstring(s: String): Int = {

    //pwwkew

    if (s.length == 0) {
      0
    }else {
      //at least first one
      var largestSubString = new mutable.HashMap[Char,Int]()
      var indexMap = new mutable.TreeMap[Int,Char]()
      largestSubString += ((s(0),0))
      indexMap += ((0,s(0)))



      var maxLength = 1
      for (j <- 1 to s.length - 1) {

        val current = s(j)

        if (largestSubString.contains(current) == false) {
          largestSubString += ((current,j))
          indexMap += ((j,current))
        }else {
          val pastIndex = largestSubString.get(current).get
          val thingsToBeRemoved = indexMap.rangeTo(pastIndex)

          for (elementIndex <- thingsToBeRemoved) {
            largestSubString.remove(elementIndex._2)
          }

          val keyList = thingsToBeRemoved.keys.toList
          for (key <- keyList) {
            indexMap.remove(key)
          }


          //largestSubString.filterInPlace((_,earlierIndex) => earlierIndex > pastIndex)
          largestSubString += ((current,j))
          indexMap += ((j,current))
        }

        if (largestSubString.size > maxLength) {
          maxLength = largestSubString.size
        }


      }

      maxLength
    }
  }

  def main(args: Array[String]): Unit = {
    println(lengthOfLongestSubstring("abcabcd"))
  }
}