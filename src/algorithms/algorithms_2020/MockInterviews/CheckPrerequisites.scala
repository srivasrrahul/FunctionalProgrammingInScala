import scala.collection.mutable

class Graph(val n : Int) {
  val nodes = new Array[mutable.HashSet[Int]](n)
  val startNodes = new mutable.HashSet[Int]
  for (j <- 0 to n-1) {
    nodes(j) = new mutable.HashSet[Int]()
    startNodes.add(j) //all nodes are begin
  }

  def addEdge(u : Int,v : Int) : Unit = {
    nodes(u).add(v)
    startNodes.remove(v) //v has a parent
  }

  def getEdges(u : Int) : Set[Int] = {
    nodes(u).toSet
  }

  def findNodesWithNoDegrees() : Set[Int] = {
    startNodes.toSet
  }
}
object Solution {
  def checkIfPrerequisite(n: Int, prerequisites: Array[Array[Int]], queries: Array[Array[Int]]): Array[Boolean] = {
    val graph = new Graph(n)
    for (preReq <- prerequisites) {
      graph.addEdge(preReq(0),preReq(1))
    }


    var time = 0
    def getNewTime() : Int = {
      time = time + 1
      time
    }

    def dfs(s : Int) : (mutable.HashSet[Int],mutable.HashMap[Int,mutable.HashSet[Int]]) = {
//      val preTimes = new mutable.HashMap[Int,Int]()
//      val postTimes = new mutable.HashMap[Int,Int]()
      val visited = new mutable.HashSet[Int]()
      val parent = new mutable.HashMap[Int,mutable.HashSet[Int]]()
      def explore(u : Int) : Unit = {
        visited.add(u)

        //preTimes += ((u,getNewTime()))
        for (v <- graph.getEdges(u)) {
          val defaultSet = parent.getOrElseUpdate(v,new mutable.HashSet[Int]())
          defaultSet.add(u)
          if (visited.contains(v) == false) {
            explore(v)
          }
        }

        //postTimes += ((u,getNewTime()))
      }

      explore(s)

      (visited,parent)
      //(preTimes,postTimes)
    }


    val visitedAndParents = new mutable.HashMap[Int,(mutable.HashSet[Int],mutable.HashMap[Int,mutable.HashSet[Int]])]()
    for (s <- graph.findNodesWithNoDegrees()) {
      val visitedParent = dfs(s)
      visitedAndParents += ((s,visitedParent))
    }


    def checkDep(u : Int,v : Int) : Boolean = {
      var depFound = false
      for ((source,(visited,parents)) <- visitedAndParents if depFound == false) {
        if (visited.contains(u) && visited.contains(v)) {
          def findParentTillSource(current : Int,target : Int) : Boolean = {
            if (current == source) {
              source == target
            }else {
              if (target == current) {
                true
              }else {
                val currentParents = parents.get(current).get
                var found = false
                for (currentParent <- currentParents if found == false) {
                  found = findParentTillSource(currentParent,target)
                }

                found
              }
            }
          }

          depFound = findParentTillSource(v,u)
        }

      }

      depFound
    }

    val retValue = new Array[Boolean](queries.length)

    for (j <- 0 to queries.length-1) {
      val query = queries(j)
      retValue(j) = checkDep(query(0),query(1))
    }

    retValue
  }

  def main(args: Array[String]): Unit = {


  }
}