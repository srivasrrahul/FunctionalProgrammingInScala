import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
class Graph {
  val nodeEdges = new mutable.HashMap[Index,List[Index]]()
  def addEdge(source : Index,dest : Index) : Unit = {
    val defaultLst = nodeEdges.getOrElseUpdate(source,List())
    nodeEdges += ((source,dest :: defaultLst))
  }

  def getNeigbours(source : Index) : List[Index] = {
    nodeEdges.getOrElse(source,List())
  }
}
object Solution {
  def wallsAndGates(rooms: Array[Array[Int]]): Unit = {
    val graph = new Graph
    val gates = new mutable.HashSet[Index]
    for (j <- 0 to rooms.length-1) {
      for (k <- 0 to rooms(0).length-1) {
        val index = new Index(j,k)
        if (rooms(j)(k) == Int.MaxValue) {
          if (j-1 >=0 && rooms(j-1)(k) != -1) {
            graph.addEdge(index,new Index(j-1,k))
          }

          if (j+1 < rooms.length && rooms(j+1)(k) != -1) {
            graph.addEdge(index,new Index(j+1,k))
          }

          if (k-1 >= 0 && rooms(j)(k-1) != -1) {
            graph.addEdge(index,new Index(j,k-1))
          }

          if (k+1 < rooms(0).length) {
            graph.addEdge(index,new Index(j,k+1))
          }
        }

        if (rooms(j)(k) == 0) {
          gates.add(index)
        }
      }
    }

    //Source is em
    def isGate(index : Index) : Boolean = {
      rooms(index.x)(index.y) == 0
    }

    val shortestPath = new mutable.HashMap[Index,Int]()
    for (node <- graph.nodeEdges.keys) {
      if (shortestPath.contains(node) == false) {
        //run bfs
        val q = new mutable.Queue[Index]()
        q.addOne(node)

        val parent = new mutable.HashMap[Index,Index]()
        val visisted = new mutable.HashSet[Index]()

        var gateFound = false
        var gateIndex : Index = null
        while (q.isEmpty == false && gateFound == false) {
          val top = q.dequeue()
          visisted.add(top)
          //path.append(top)
          if (isGate(top)) {
            //path.append(top)
            q.dequeueAll(_ => true)
            gateFound = true
            gateIndex = top
          }else {

            for (neigbour <- graph.getNeigbours(top)) {
              if (visisted.contains(neigbour) == false) {
                parent += ((neigbour,top))
                q.addOne(neigbour)
              }
            }
          }
        }

        if (gateFound == true) {
          var current = gateIndex
          var pathLen = 0
          do {
            shortestPath += ((current,pathLen))
            current = parent.getOrElse(current,null)
            pathLen = pathLen + 1
          }while (current != null)
        }
      }
    }

    println(shortestPath)
    for ((source,len) <- shortestPath) {
      rooms(source.x)(source.y) = len
    }
  }
}