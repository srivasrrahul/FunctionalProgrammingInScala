import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def minWindow(s: String, t: String): String = {
    val cache = new mutable.HashMap[Char,mutable.TreeSet[Int]]()


    for (j <- 0 to s.length-1) {
      val ch = s(j)
      val set = cache.getOrElseUpdate(ch,new mutable.TreeSet[Int]())
      set.add(j)
    }

    def findNewRange(j : Int,k : Int,char: Char,map : Map[Char,mutable.TreeSet[Int]]) : Option[(Int,Int)] = {
      val relevantTree = map.get(char)
      if (relevantTree.isEmpty) {
        None
      }else {
        //first first in mid
        val midTree = relevantTree.get.range(j,k)
        if (midTree.isEmpty == false) {
          //remove the same
          midTree.remove(midTree.head)
          Some((j,k))
        }else {
          //Search on left
          val leftTree = relevantTree.get.range(0,j)
          var leftOption : Option[(Int,Int)] = None
          if (leftTree.isEmpty == false) {
            leftOption = Some((leftTree.last,k))
          }

          val rightTree = relevantTree.get.rangeFrom(k+1)
          var rightOption : Option[(Int,Int)] = None
          if (rightTree.isEmpty == false) {
            rightOption = Some((j,rightTree.head))
          }

          (leftOption,rightOption) match {
            case (None,None) => None
            case (None,_) => {
              rightTree.remove(rightTree.head)
              rightOption
            }
            case (_,None) => {
              leftTree.remove(leftTree.last)
              leftOption
            }
            case (Some((x1,x2)),Some((y1,y2))) => {
              val diff1 = x2-x1+1
              val diff2 = y2-y1+1
              if (diff1 < diff2) {
                leftTree.remove(leftTree.last)
                leftOption
              }else {
                rightTree.remove(rightTree.head)
                rightOption
              }
            }
          }
        }
      }
    }

    val possibleLsts = new ListBuffer[((Int,Int),Map[Char,mutable.TreeSet[Int]])]
    if (cache.contains(s.head)) {
      val indexes = cache.get(s.head).get
      for (index <- indexes) {
        possibleLsts.append(((index,index),cache.toMap))
      }
    }

    var minRange : Option[(Int,Int)] = None
    for (possible <- possibleLsts) {
      var ((j,k),map) = possible
      var found = true
      for (tIndex <- 1 to t.length-1 if found == true) {
        val updatedRange = findNewRange(j,k,t(tIndex),map)
        if (updatedRange.isDefined == false) {
          found = false
        }else {
          j = updatedRange.get._1
          k = updatedRange.get._2
        }
      }

      if (found == true) {
        val localDiff = k-j+1
        if (minRange.isDefined) {
          val oldDiff = minRange.get._2 - minRange.get._1 + 1
          if (localDiff < oldDiff) {
            minRange = Some((j,k))
          }
        }else {
          minRange = Some((j,k))
        }
      }
    }

    if (minRange.isDefined) {
      s.substring(minRange.get._1,minRange.get._2+1)
    }else {
      ""
    }
  }
}