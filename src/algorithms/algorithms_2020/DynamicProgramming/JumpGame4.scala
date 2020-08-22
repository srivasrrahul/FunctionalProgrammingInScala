import java.util

import scala.collection.mutable

object Solution {
  def minJumps(arr: Array[Int]): Int = {
    val indexValues = new mutable.HashMap[Int,mutable.HashSet[Int]]()

    for (j <- 0 to arr.length-1) {
      val currentValue = arr(j)
      val defaultSet = indexValues.getOrElseUpdate(currentValue,new mutable.HashSet[Int]())
      defaultSet.add(j)
    }

    val queue = new mutable.Queue[(Int,Int)]()
    val visitedLength = new mutable.HashMap[Int,Int]()

    queue.addOne((0,0))

    val parent = new mutable.HashMap[Int,Int]()
    var endFound = false
    while (queue.isEmpty == false && endFound == false) {
      val (top,distance) = queue.dequeue()
      visitedLength += ((top,distance))

      val nextDistance = 1+distance
      if (top + 1 < arr.length) {

        if (visitedLength.contains(top+1)) {
          val lastDistance = visitedLength.get(top+1).get
          if (nextDistance < lastDistance) {
            visitedLength += ((top+1,nextDistance))
            parent += ((top + 1, top))
            queue.append((top + 1,nextDistance))
          }
        }else {
          visitedLength += ((top+1,nextDistance))
          parent += ((top + 1, top))
          queue.append((top + 1,nextDistance))
        }
      }

      if (top - 1 >= 0) {
        if (visitedLength.contains(top-1)) {
          val lastDistance = visitedLength.get(top-1).get
          if (nextDistance < lastDistance) {
            visitedLength += ((top-1,nextDistance))
            parent += ((top -1, top))
            queue.append((top -1,nextDistance))
          }
        }else {
          visitedLength += ((top-1,nextDistance))
          parent += ((top-1, top))
          queue.append((top-1,nextDistance))
        }
      }

      for (similarIndex <- indexValues.get(arr(top)).get) {
        if (visitedLength.contains(similarIndex)) {
          val existingDistance = visitedLength.get(similarIndex).get
          if (nextDistance < existingDistance) {
            visitedLength += ((similarIndex,nextDistance))
            parent += ((similarIndex, top))
            queue.append((similarIndex,nextDistance))
          }
        }else {
          visitedLength += ((similarIndex,nextDistance))
          parent += ((similarIndex, top))
          queue.append((similarIndex,nextDistance))
        }
      }
    }

    println(parent)
    var countParent = 0
    var current = arr.length-1
    while (parent.contains(current)) {
      countParent = countParent+1
      current = parent.get(current).get
    }

    countParent

  }
}