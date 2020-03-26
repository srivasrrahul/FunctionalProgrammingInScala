import scala.collection.mutable
import util.control.Breaks._

object Solution {
//  def isSubsequence(s: String, t: String): Boolean = {
//    val dict = new mutable.HashMap[Char, mutable.TreeSet[Int]]()
//
//    var j = 0
//    for (ch <- t) {
//      dict.get(ch) match {
//        case Some(set) => {
//          set.add(j)
//        }
//        case None => {
//          val set = new mutable.TreeSet[Int]()
//          set.add(j)
//          dict += ((ch, set))
//        }
//      }
//
//      j = j + 1
//    }
//
//
//    def findNext(ch: Char, minIndex: Int): Option[mutable.TreeSet[Int]] = {
//      dict.get(ch) match {
//        case None => None
//        case Some(set) => {
//          val retValue = set.rangeFrom(minIndex)
//          if (retValue.size == 0) {
//            None
//          }else {
//            Some(retValue)
//          }
//        }
//      }
//    }
//
//    def isSubSeqItr(xCurrent : Int,yMin : Int) : Boolean = {
//      if (xCurrent == s.length-1) {
//        findNext(s.charAt(xCurrent),yMin) match {
//          case None => {
//            false
//          }
//          case _ => {
//            true
//          }
//        }
//      }else {
//        findNext(s.charAt(xCurrent),yMin) match {
//          case None => {
//            false
//          }
//          case Some(indexSet) => {
//            var result = false
//            breakable {
//
//              for (index <- indexSet) {
//                val res = isSubSeqItr(xCurrent+1,index+1)
//                if (res == true) {
//                  result = true
//                  break
//                }
//              }
//
//
//            }
//
//            result
//          }
//        }
//      }
//    }
//
//    if (s.length == 0) {
//      true
//    }else {
//      isSubSeqItr(0,0)
//    }
//
//  }

  def isSubsequence(s: String, t: String): Boolean = {
    var sIndex = 0
    var tIndex = 0

    while (sIndex < s.length && tIndex < t.length) {
      if (s.charAt(sIndex) == t.charAt(tIndex)) {
        sIndex = sIndex + 1
        tIndex = tIndex + 1
      }else {
        tIndex = tIndex + 1
      }
    }

    if (sIndex >= s.length) {
      true
    }else {
      false
    }
  }



  def main(args: Array[String]): Unit = {
    println(isSubsequence("axc","ahbgdc"))
  }
}