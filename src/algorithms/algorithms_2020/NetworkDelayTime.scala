import scala.collection.mutable

class Graph(val nodeCount : Int) {
  val nodeArr = new Array[mutable.HashMap[Int,Int]](nodeCount)
  for (j <- 0 to nodeArr.length-1) {
    nodeArr(j) = new mutable.HashMap[Int,Int]()
  }

  def addEdge(source : Int,dest : Int,dist : Int) : Unit = {
    nodeArr(source) += ((dest,dist))
  }

  def getNeigbours(source : Int) : mutable.HashMap[Int,Int] = {
    nodeArr(source)
  }

}
object Solution {
  def networkDelayTime(times: Array[Array[Int]], N: Int, K: Int): Int = {
    val graph = new Graph(N)
    for (time <- times) {
      graph.addEdge(time(0)-1,time(1)-1,time(2))
    }

    println(graph.nodeArr.mkString(","))

    val visited = new mutable.HashSet[Int]()
    val distance = new mutable.HashMap[Int,Int]()

    val pq = new mutable.PriorityQueue[(Int,Int)]()(new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        y._2.compareTo(x._2)

      }
    })
    pq.addOne((K-1,0))
    distance += ((K-1,0))

    while (pq.isEmpty == false) {
      val top = pq.dequeue()
      if (visited.contains(top._1) == false) {
        println("top is " + top._1)
        visited.add(top._1)

        //println(top)
        val neigbours = graph.getNeigbours(top._1)
        for (neigbour <- neigbours) {
          val newDistance = distance.get(top._1).get + neigbour._2
          distance.get(neigbour._1) match {
            case None => {
              distance += ((neigbour._1,newDistance))
              pq.addOne((neigbour._1,newDistance))
            }
            case Some(oldPathLength) => {
              if (newDistance < oldPathLength) {
                distance += ((neigbour._1,newDistance))
                pq.addOne((neigbour._1,newDistance))
              }
            }
          }
        }
      }
    }

    //println(distance)
    if (distance.size == N) {
      distance.values.max
    }else {
      -1
    }
  }

  def main(args: Array[String]): Unit = {
    val times = Array(Array(2,1,1),Array(2,3,1),Array(3,4,1))
    //val times = Array[Array[Int]]()
    println(networkDelayTime(times,4,2))
    //println(networkDelayTime(times,4,2))
  }
}