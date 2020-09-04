import scala.collection.mutable

//case class NodeId(val x : Int,val y : Int)
//class Graph {
//  val nodes = new mutable.HashMap[NodeId,mutable.HashSet[NodeId]]()
//
//  def addNode(n1 : NodeId,n2 : NodeId) : Unit = {
//    val s1 = nodes.getOrElseUpdate(n1,new mutable.HashSet[NodeId]())
//    s1.add(n2)
//
//    val s2 = nodes.getOrElseUpdate(n2,new mutable.HashSet[NodeId]())
//    s2.add(n1)
//  }
//
//  def getNext(n1 : NodeId) : Set[NodeId] = {
//    nodes.getOrElse(n1,new mutable.HashSet[NodeId]()).toSet
//  }
//}
object Solution {
  def maxDistance(grid: Array[Array[Int]]): Int = {
    //run bfs from each water land and stor in TreeSet
    val rows = grid.length
    val cols = grid(0).length

    val totalVertex = rows*cols

    val dist = Array.ofDim[Int](totalVertex,totalVertex)

    def getIndex(vertexId : Int) : (Int,Int) = {
      (vertexId/cols,vertexId % cols)
    }

    for (j <- 0 to totalVertex-1) {
      for (k <- 0 to totalVertex-1) {
        if (j != k) {
          dist(j)(k) = Int.MaxValue
        }else {
          dist(j)(k) = 0
        }
      }
    }

    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        val current = j*cols + k
        if (j-1>= 0) {
          val aboveId = (j-1)*cols + k
          dist(current)(aboveId) = 1
        }

        if (j+1 < rows) {
          val belowId = (j+1)*cols + k
          dist(current)(belowId) = 1
        }

        if (k-1 >= 0) {
          val left = j*cols + (k-1)
          dist(current)(left) = 1
        }

        if (k+1 < cols) {
          val right = j*cols + (k+1)
          dist(current)(right) = 1
        }
      }
    }

    for (k <- 0 to totalVertex-1) {
      for (i <- 0 to totalVertex-1) {
        for (j <- 0 to totalVertex-1) {
          //relax the edge
          if (dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue) {
            if (dist(i)(k) + dist(k)(j) < dist(i)(j)) {
              dist(i)(j) = dist(i)(k) + dist(k)(j)
            }
          }
        }
      }
    }

    //Now you have all pair distances
    var minDistances = new mutable.ArrayBuffer[Int]
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {

        if (grid(j)(k) == 0) { //its water
          val vertex = j*cols + k
          var minDistance = Int.MaxValue
          for (otherVertx <- 0 to totalVertex-1 if otherVertx != vertex) {
            val (j1,k1) = getIndex(otherVertx)
            if (grid(j1)(k1) == 1) { //its land
              val distance = dist(vertex)(otherVertx)
              if (distance < minDistance) {
                minDistance = distance
              }
            }
          }

          if (minDistance != Int.MaxValue) {
            minDistances.append(minDistance)
          }
        }
      }
    }



    if (minDistances.isEmpty == false) {
      minDistances.max

    }else {
      -1
    }




  }
}