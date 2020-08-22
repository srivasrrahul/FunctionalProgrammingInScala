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

    //val parent = new mutable.HashMap[Int,Int]()
    var endFound = false
    while (queue.isEmpty == false && endFound == false) {
      val (top,distance) = queue.dequeue()
      visitedLength += ((top,distance))

      if (top == arr.length-1) {
        endFound = true
      }else {

        val nextDistance = 1+distance
        if (indexValues.contains(arr(top))) {
          for (similarIndex <- indexValues.get(arr(top)).get) {
            if (visitedLength.contains(similarIndex)) {

            }else {
              visitedLength += ((similarIndex,nextDistance))
              //parent += ((similarIndex, top))
              queue.append((similarIndex,nextDistance))
            }
          }
        }

        indexValues.remove(arr(top))


        if (top + 1 < arr.length) {
          if (visitedLength.contains(top+1)) {

          }else {
            visitedLength += ((top+1,nextDistance))
            //parent += ((top + 1, top))
            queue.append((top + 1,nextDistance))
          }
        }

        if (top - 1 >= 0) {
          if (visitedLength.contains(top-1)) {

          }else {
            visitedLength += ((top-1,nextDistance))
            queue.append((top-1,nextDistance))
          }
        }


      }
    }

    visitedLength.get(arr.length-1).get

  }
}