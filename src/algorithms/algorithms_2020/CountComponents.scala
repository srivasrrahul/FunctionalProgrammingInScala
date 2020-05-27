import scala.collection.mutable

class Graph {
  val nodeEdges = new mutable.HashMap[Int,mutable.HashSet[Int]]()

  def addEdge(array: Array[Int]) : Unit = {
    val source = array(0)
    val dest = array(1)

    val sourceNeigbours = nodeEdges.getOrElseUpdate(source,new mutable.HashSet[Int]())
    sourceNeigbours.add(dest)

    val destNeigbours = nodeEdges.getOrElseUpdate(dest,new mutable.HashSet[Int]())
    destNeigbours.add(source)


  }

  def getNeigbours(source : Int): Set[Int] = {
    nodeEdges.get(source) match {
      case None => Set()
      case Some(neigbours) => neigbours.toSet
    }
  }
}
object Solution {
  def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
    val graph = new Graph

    for (edge <- edges) {
      graph.addEdge(edge)
    }

    val visited = new mutable.HashSet[Int]()
    var count = 0
    for (j <- 0 to n-1) {
      if (visited.contains(j) == false) {
        count = count+1
        val q = new mutable.Queue[Int]()
        q.addOne(j)
        while (q.isEmpty == false) {
          val top = q.dequeue()
          visited.add(top)

          for (neigbour <- graph.getNeigbours(top)) {
            if (visited(neigbour) == false) {
              q.addOne(neigbour)
            }
          }
        }
      }
    }

    count
  }
}