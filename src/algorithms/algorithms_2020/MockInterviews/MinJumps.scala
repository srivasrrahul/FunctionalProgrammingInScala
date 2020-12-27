import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def minJumps(arr: Array[Int]): Int = {
    val valIndex = new mutable.HashMap[Int,mutable.HashSet[Int]]()
    for (j <- 0 to arr.length-1) {
      val defSet = valIndex.getOrElseUpdate(arr(j),new mutable.HashSet[Int]())
      defSet.add(j)
    }



    val visited = new mutable.HashSet[Int]()
    val distance = new mutable.HashMap[Int,Int]() //parent of j to k
    for (j <- 0 to arr.length-1) {
      distance.addOne((j,Int.MaxValue))
    }

    distance += ((0,0))

    val pq = mutable.PriorityQueue.empty[(Int,Int)](new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        y._2.compareTo(x._2)
      }
    })

    pq.addOne((0,0))

    var endFound = false
    while (pq.isEmpty == false && endFound == false) {
      val (topId,topDist) = pq.dequeue()

      if (topId+1 < arr.length) {
        if (distance.get(topId+1).get == Int.MaxValue || (distance.get(topId+1).get > (topDist+1))) {
          distance += ((topId+1,topDist+1))
          pq.addOne((topId+1,topDist+1))
        }
      }

      if (topId-1 >= 0) {
        if (distance.get(topId-1).get == Int.MaxValue || distance.get(topId-1).get> (topDist+1)) {
          distance += ((topId-1,topDist+1))
          pq.addOne((topId-1,topDist+1))
        }
      }


      val topValue = arr(topId)
      if (valIndex.contains(topValue)) {
        for (next <- valIndex.get(topValue).get) {
          if (next != topId) {
            if (distance.get(next).get == Int.MaxValue || distance.get(next).get> (topDist+1)) {
              distance += ((next,topDist+1))
              pq.addOne((next,topDist+1))
            }
          }
        }
      }

      valIndex.remove(topValue)
      if (topId == arr.length-1) {
        endFound = true
      }
    }

    distance.get(arr.length-1).get
  }
}