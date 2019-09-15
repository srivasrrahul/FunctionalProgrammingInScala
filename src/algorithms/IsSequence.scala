import util.control.Breaks._
object Solution {
//  def isSubsequence(s: String, t: String): Boolean = {
//    @scala.annotation.tailrec
//    def itr(sourceIndex : Int,destIndex : Int) : Boolean = {
//      if (sourceIndex == s.length && destIndex <= t.length) {
//        //All consumed from source
//        true
//      }else {
//        if (sourceIndex < s.length && destIndex == t.length) {
//          //All dest completed but source is pending
//          false
//        }else {
//          //println(s.charAt(sourceIndex) + " " + t.charAt(destIndex))
//          if (s.charAt(sourceIndex) == t.charAt(destIndex)) {
//            itr(sourceIndex+1,destIndex+1)
//          }else {
//            itr(sourceIndex,destIndex+1)
//          }
//        }
//      }
//    }
//
//    itr(0,0)
//  }

  def isSubsequence(s: String, t: String) : Boolean = {

    var sourceIndex = 0
    var destIndex = 0
    while (sourceIndex < s.length && destIndex < t.length) {
      if (s.charAt(sourceIndex) == t.charAt(destIndex)) {
        sourceIndex = sourceIndex + 1
        destIndex = destIndex + 1
      }else {
        destIndex = destIndex + 1
      }
    }

    if (sourceIndex >= s.length) {
      true
    }else {
      false
    }

  }

  def main(args: Array[String]): Unit = {
    println(isSubsequence("abc","ahbgdc"))
  }
}