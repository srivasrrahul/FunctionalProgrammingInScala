import scala.collection.mutable

object Solution {
  def findTheDifference(s: String, t: String): Char = {
    val charCountS = new mutable.HashMap[Char,Int]()
    for (ch <- s) {
      val defaultCount = charCountS.getOrElseUpdate(ch,0)
      charCountS += ((ch,defaultCount+1))
    }

    val charCountT = new mutable.HashMap[Char,Int]()
    for (ch1 <- t) {
      val defaultCount = charCountT.getOrElseUpdate(ch1,0)
      charCountT += ((ch1,defaultCount+1))
    }

    var found = false
    var foundChar = '0'
    for ((ch,chCount) <- charCountT if found == false) {
      charCountS.get(ch) match {
        case None => {
          found = true
          foundChar = ch
        }
        case Some(tCount) => {
          if (tCount != chCount) {
            found = true
            foundChar = ch
          }
        }
      }
    }

    foundChar
  }
}