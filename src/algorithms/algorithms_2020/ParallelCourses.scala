import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph(val n : Int) {
  val edges = new mutable.HashMap[Int,mutable.HashSet[Int]]()
  for (j <- 0 to n-1) {
    edges += ((j,new mutable.HashSet[Int]()))
  }

  def addEdge(edge : Array[Int]) : Unit = {
    val defaultSet = edges.get(edge(0)-1).get
    defaultSet.add(edge(1)-1)
  }

  def getNeigbours(source : Int) : Set[Int] = {
    edges.get(source).get.toSet
  }

  def isEdge(source : Int,dest : Int) : Boolean = {
    edges.get(source).get.contains(dest)
  }

  def findSources() : List[Int] = {
    val sources = new mutable.HashSet[Int]()
    for ((node,_) <- edges) {
      sources.add(node)
    }

    for ((node,edges) <- edges) {
      for (edge <- edges) {
        sources.remove(edge)
      }
    }

    sources.toList
  }

  def removeNode(source : Int) : Unit = {
    edges.remove(source)
  }
}
object Solution {
  def minimumSemesters(N: Int, relations: Array[Array[Int]]): Int = {
    var semester = 0
    val graph = new Graph(N)
    for (relation <- relations) {
      graph.addEdge(relation)
    }

    var time = 0
    val preTime = new Array[Int](N)
    val postTime = new Array[(Int,Int)](N)
    for (j <- 0 to postTime.length-1) {
      postTime(j) = (0,j)
    }
    def getNewTime() : Int = {
      val newTime = time + 1
      time = newTime
      newTime
    }

    val visited = new mutable.HashSet[Int]()
    var cycleExists = false

    def explore(source : Int) : Unit = {
      visited.add(source)
      preTime(source) = getNewTime()
      val neigbours = graph.getNeigbours(source)
      for (neigbour <- neigbours) {
        if (visited.contains(neigbour) == false) {
          explore(neigbour)
        }else {
          if (postTime(neigbour)._1 == 0) {
            cycleExists = true
          }else {

          }
        }
      }

      postTime(source) = (getNewTime(),source)
    }

    var groupId = N
    for (j <- 0 to N-1) {
      if (visited.contains(j) == false) {
        explore(j)
      }

    }

    if (cycleExists == false) {
      var noSource = false
      var semester = 0
      while (noSource == false) {
        val sources = graph.findSources()
        if (sources.isEmpty == false) {
          semester = semester + 1
          for (source <- sources) {
            graph.removeNode(source)
          }
        }else {
          noSource = true
        }

      }

      semester
    }else {
      -1
    }


  }
}