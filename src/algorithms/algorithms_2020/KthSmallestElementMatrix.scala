import scala.collection.mutable

case class Tuple(val x : Int,val y : Int)
object Solution {
  def kthSmallest(matrix: Array[Array[Int]], K: Int): Int = {
    val pq = mutable.PriorityQueue.empty[(Int,Tuple)](new Ordering[(Int,Tuple)] {
      override def compare(x: (Int, Tuple), y: (Int, Tuple)): Int = {
        y._1.compareTo(x._1)
      }
    })

    val maxRow = matrix.length-1
    val maxCol = matrix.length-1

    val visited = new mutable.HashSet[Tuple]()
    //visited.add(new Tuple(0,0))
//    visited.add(new Tuple(0,1))
//    visited.add(new Tuple(1,0))

    pq.addOne((matrix(0)(0),new Tuple(0,0)))
    //pq.addOne((matrix(0)(1),new Tuple(0,1)))
    //pq.addOne((matrix(1)(0),new Tuple(1,0)))

    def itr(k : Int) : Int = {
      if (k == 1) {
        val (topVal,topTuple) = pq.dequeue()
        var topCurrentTuple = topTuple
        if (visited.contains(topCurrentTuple) == true) {
          while (visited.contains(topCurrentTuple) == true) {
            val (x,y) = pq.dequeue()
            topCurrentTuple = y
          }
        }

        matrix(topCurrentTuple.x)(topCurrentTuple.y)
      }else {
        val (topVal,topTuple) = pq.dequeue()
        var topCurrentTuple = topTuple
        if (visited.contains(topCurrentTuple) == true) {
          while (visited.contains(topCurrentTuple) == true) {
            val (x,y) = pq.dequeue()
            topCurrentTuple = y
          }
        }

        visited.add(topCurrentTuple)
        //Add its row and column
        val x = topCurrentTuple.x
        val y = topCurrentTuple.y

        //Col till maxCol
        if (y+1 <= maxCol) {
          val newTuple = new Tuple(x,y+1)
          pq.addOne((matrix(x)(y+1),newTuple))
        }
//        for (j <- y+1 to maxCol) {
//          val newTuple = new Tuple(x,j)
//
//        }

        if (x+1 <= maxRow) {
          val newTuple = new Tuple(x+1,y)
          pq.addOne((matrix(x+1)(y),newTuple))
        }
//        for (j <- x+1 to maxRow) {
//          val newTuple = new Tuple(j,y)
//          pq.addOne((matrix(j)(y),newTuple))
//        }

        itr(k-1)
      }
    }

    itr(K)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(1,10,12),Array(2,20,25),Array(3,21,102))
    println(kthSmallest(arr,5))
  }
}