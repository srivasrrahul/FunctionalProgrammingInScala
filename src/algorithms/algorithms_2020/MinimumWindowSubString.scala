import scala.collection.mutable

object Solution {
  def minWindow(s: String, t: String): String = {
    val targetCharCount = new mutable.HashMap[Char,Int]()
    for (ch <- t) {
      val defaultCount = targetCharCount.getOrElse(ch,0)
      targetCharCount += ((ch,defaultCount+1))
    }

    val charCountMatrix = new Array[Map[Char,Int]](s.length)
    val newMap = new mutable.HashMap[Char,Int]()
    newMap += ((s(0),1))
    charCountMatrix(0) = newMap.toMap

    for (j <- 1 to s.length-1 ) {
      val newMap = charCountMatrix(j-1)
      val defaultCount = charCountMatrix(j-1).getOrElse(s(j-1),0)
      charCountMatrix(j) = newMap.+((s(j),defaultCount+1))
    }

    def equal(sourceMap : mutable.HashMap[Char,Int]) : Boolean = {
      var notEqual = false
      for ((tCh,tCount) <- targetCharCount if notEqual == false) {
        if (sourceMap.contains(tCh)) {
          if (sourceMap.get(tCh).get < tCount) {
            notEqual = true
          }
        }else {
          notEqual = true
        }
      }

      !notEqual
    }

    //println(targetCharCount)
    var minLength : Option[(Int,Int)] = None
    for (j <- 0 to s.length-1) {
      for (k <- j to s.length-1) {
        val localCharCount = new mutable.HashMap[Char,Int]()
        for (x <- j to k) {
          val defaultCount = localCharCount.getOrElse(s(x),0)
          localCharCount += ((s(x),defaultCount+1))
        }

        //println(localCharCount)
        if (equal(localCharCount)) {
          val len = k-j+1
          minLength match {
            case None => {
              minLength = Some((j,k))
            }
            case Some((a,b)) => {
              if (len < (b-a+1)) {
                minLength = Some((j,k))
              }
            }
          }
        }
      }
    }
    minLength match {
      case None => ""
      case Some((a,b)) => {
        s.substring(a,b+1)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    println(minWindow("ADOBECODEBANC","ABC"))
  }
}