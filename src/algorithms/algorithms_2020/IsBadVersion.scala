abstract class VersionControl {
  def isBadVersion(version: Int): Boolean
}

import scala.util.control.Breaks._
class Solution extends VersionControl {
  def firstBadVersion(n: Int): Int = {
    var current = n
    var first = 1



    //Assumes at least one exists
    var lastFound = -1
    breakable {
      while (true) {
        if (current <= first) {
          lastFound = first
          break
        } else {
          val mid = first + (current - first)/2
          val badInMid = isBadVersion(mid)
          if (badInMid == true) {
            current = badInMid
          }else {
            first = current+1
          }
        }
      }
    }

    lastFound
  }
}