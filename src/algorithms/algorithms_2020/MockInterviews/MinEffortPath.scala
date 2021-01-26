import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
object Solution {
  def minimumEffortPath(heights: Array[Array[Int]]): Int = {
    val rows = heights.length
    val cols = heights(0).length

    val last = new Index(rows-1,cols-1)

    def next(current : Index) : List[Index] = {
      val lst = new ListBuffer[Index]
      val x = current.x
      val y = current.y

      if (x+1 < rows) {
        lst.append(new Index(x+1,y))
      }

      if (x-1 >=0) {
        lst.append(new Index(x-1,y))
      }

      if (y+1 < cols) {
        lst.append(new Index(x,y+1))
      }

      if (y-1 >= 0) {
        lst.append(new Index(x,y-1))
      }

      lst.toList
    }


    def getDist(i1 :Index,i2 : Index) : Int = {
      math.abs(heights(i1.x)(i1.y) - heights(i2.x)(i2.y))
    }

    val pq = mutable.PriorityQueue.empty[(Int,Index)](new Ordering[(Int,Index)] {
      override def compare(x: (Int, Index), y: (Int, Index)): Int = {
        y._1.compare(x._1)
      }
    })

    val distance = new mutable.HashMap[Index,Int]()
    distance += ((new Index(0,0),0))

    pq.addOne((0,new Index(0,0)))

    var found = false
    while (pq.isEmpty == false && found == false) {
      val (topDist,topIndex) = pq.dequeue()

      if (topIndex == last) {
        //
        found = true
      }else {
        for (n <- next(topIndex)) {
          val newDist = math.max(topDist,getDist(topIndex,n))
          if (distance.contains(n) == false) {
            distance += ((n,newDist))
            pq.addOne((newDist,n))
          }else {
            val existingDist = distance.get(n).get
            if (newDist < existingDist) {
              distance += ((n,newDist))
              pq.addOne((newDist,n))
            }
          }

          //println(n + " " + distance)
        }

      }
    }

    //println(distance)
    distance.get(last).get
  }
}