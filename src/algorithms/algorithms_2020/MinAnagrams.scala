import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def minSteps(s: String, t: String): Int = {
    val sMap = new mutable.HashMap[Char,Int]()
    val tMap = new mutable.HashMap[Char,Int]()


    for (ch <- s) {
      val defaultCount = sMap.getOrElseUpdate(ch,0)
      sMap += ((ch,defaultCount+1))

    }

    for (ch <- t) {
      val defaultCount = tMap.getOrElseUpdate(ch,0)
      tMap += ((ch,defaultCount+1))
    }

    //remove common elements

    for (ch <- s) {
      if (tMap.contains(ch)) {
        val count = tMap.get(ch).get
        if (count == 1) {
          tMap.remove(ch)
        }else {
          tMap += ((ch,count-1))
        }
      }
    }

    //No elements are common in sMap and tMap
    var count = 0
    for ((ch,charCount) <- tMap) {
      count = count + charCount
    }

    count



  }

  def main(args: Array[String]): Unit = {

    println(minSteps("anagram","mangaar"))
  }
}