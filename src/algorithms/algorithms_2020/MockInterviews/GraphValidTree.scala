import scala.collection.mutable

class Graph(val n : Int) {
  val arr = new Array[mutable.HashSet[Int]](n)
  for (j <- 0 to arr.length-1) {
    arr(j) = new mutable.HashSet[Int]()
  }

  def addEdge(u : Int,v : Int) : Unit = {
    arr(u).add(v)
    arr(v).add(u)
  }

  def getNeigbours(u : Int) : Set[Int] = {
    arr(u).toSet
  }
}
object Solution {
  def validTree(n: Int, edges: Array[Array[Int]]): Boolean = {
    val graph = new Graph(n)
    for (edge <- edges) {
      graph.addEdge(edge(0),edge(1))
    }

    val visited = new mutable.HashSet[Int]()
    //val preMap = new mutable.HashSet[Int]()
    val postMap = new mutable.HashSet[Int]()

    var time = 0
    // def getNewTime() : Int = {
    //   time = time+1
    //   time
    // }
    var cycleFound = false
    val parent = new mutable.HashMap[Int,Int]()
    def explore(u : Int) : Unit = {
      visited.add(u)
      val neigbours = graph.getNeigbours(u)
      //preMap += ((u,getNewTime()))
      for (v <- neigbours) {
        if (visited.contains(v) == false) {
          parent += ((v,u))
          explore(v)
        }else {
          if (postMap.contains(v) == false && parent.get(u).get != v) {
            //pre processed but not post
            cycleFound = true
          }
        }
      }

      postMap.add(u)


    }


    explore(0)


    cycleFound == false && visited.size == n
  }
}