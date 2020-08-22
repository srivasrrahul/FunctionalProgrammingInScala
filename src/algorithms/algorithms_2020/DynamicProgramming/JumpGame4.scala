import java.util

import scala.collection.mutable

object Solution {
  def minJumps(arr: Array[Int]): Int = {
    val indexValues = new mutable.HashMap[Int,mutable.TreeSet[Int]]()

    for (j <- 0 to arr.length-1) {
      val currentValue = arr(j)
      val defaultSet = indexValues.getOrElseUpdate(currentValue,new mutable.TreeSet[Int]())
      defaultSet.add(j)
    }

    def itr(currentIndex : Int,visited : Set[Int]) : Option[Int] = {
      if (currentIndex == arr.length-1) {
        Some(0)
      }else {
        val next1 = currentIndex+1
        val next2 = currentIndex-1
        val allSets = indexValues.get(arr(currentIndex)).get

        val pathLengths = new mutable.ArrayBuffer[Int]()

        val newSet = visited.+(currentIndex)
        if (next1 < arr.length && visited.contains(next1) == false) {
          itr(next1,newSet) match {
            case Some(c1) => pathLengths.append(c1)
            case None => {}
          }
        }

        if (next2 >= 0 && visited.contains(next2) == false) {
          itr(next2,newSet) match {
            case Some(c1) => pathLengths.append(c1)
            case None => {}
          }
        }

        for (sameValIndex <- allSets) {
          if (visited.contains(sameValIndex) == false) {
            itr(sameValIndex,newSet) match {
              case Some(c1) => pathLengths.append(c1)
              case None => {}
            }
          }
        }

        if (pathLengths.isEmpty) {
          None
        }else {
          Some(pathLengths.min)
        }

      }
    }

    itr(0,Set()) match {
      case Some(c) => c
      case _ => -1
    }
  }
}