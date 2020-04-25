import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._
import scala.io.Source

case class Edge(val u : Int,val v : Int)
class Graph(val vertexCount : Int) {
  val nodeEdges = new Array[mutable.HashSet[Int]](vertexCount)
  for (j <- 0 to vertexCount-1) {
    nodeEdges(j) = new mutable.HashSet[Int]()
  }

  def addEdge(u : Int,v : Int) : Unit = {
    nodeEdges(u).add(v)
    nodeEdges(v).add(u)
  }

  def removeEdge(u : Int,v : Int) : Unit = {
    nodeEdges(u).remove(v)
    nodeEdges(v).remove(u)
  }

  def getEdges(u : Int) : Set[Int] = {
    nodeEdges(u).toSet
  }


  def dfsPrePost() : Array[Set[Int]] = {
    val visited = new mutable.HashSet[Int]()
    val prePost = new Array[(Int,Int)](vertexCount)
    for (j <- 0 to prePost.length-1) {
      prePost(j) = (0,0)
    }

    val path = new Array[Int](vertexCount)
    for (j <- 0 to path.length-1) {
      path(j) = -1
    }

    var time = 0
    def incTime() : Int = {
      time = time + 1
      time
    }

    //val dualPaths = new ListBuffer[Set[Int]]()

    val dualPathIds = new Array[Set[Int]](vertexCount)
    for (j <- 0 to dualPathIds.length-1) {
      dualPathIds(j) = Set[Int]()
    }

    var dualPathId = 1

    def getNewDualPathId() : Int = {
      dualPathId = dualPathId + 1
      dualPathId
    }

    def isStronglyConnected(source : Int,dest : Int) : Option[Int] = {
      val intersection = dualPathIds(source).intersect(dualPathIds(dest))
      if (intersection.isEmpty) {
        None
      }else {
        Some(intersection.head)
      }
    }

    def explore(current : Int) : Unit = {
      visited.add(current)

      prePost(current) = (incTime(),0)

      for (dest <- getEdges(current)) {
        //println("In edge " + current + " " + dest)
        if (visited.contains(dest) == false) {
          path(dest) = current
          explore(dest)
        }else {
          val destPreValue = prePost(dest)._1
          val destPrePostValue = prePost(dest)._2

          if (destPreValue != 0 && destPrePostValue == 0 && path(current) != dest) {
            //println("backedge " + current + " " + dest)
            var parentCurrent = path(current)

            var anyIdFound = false
            var commonId = -1
            breakable {
              while (parentCurrent != -1 && parentCurrent != dest) {

                isStronglyConnected(dest, parentCurrent) match {
                  case Some(existingGroup) => {
                    anyIdFound = true
                    commonId= existingGroup
                    break
                  }
                  case _ => {

                  }
                }
                parentCurrent = path(parentCurrent)
              }
            }

            parentCurrent = path(current)

            if (anyIdFound == true) {

              dualPathIds(current) = dualPathIds(current).+(commonId)
              while (parentCurrent != -1 && parentCurrent != dest) {

                dualPathIds(parentCurrent) = dualPathIds(parentCurrent).+(commonId)
                parentCurrent = path(parentCurrent)
              }

              dualPathIds(dest) = dualPathIds(dest).+(commonId)

            }else {
              //println("Here ")
              val newId = getNewDualPathId()
              dualPathIds(current) = dualPathIds(current).+(newId)
              dualPathIds(dest) = dualPathIds(dest).+(newId)

              //println("Id shared " + current + " " + dest + " pc = " +  parentCurrent + " path " + path.mkString(","))

              while (parentCurrent != -1 && (parentCurrent != dest)) {

                //println("Id shared parent " + parentCurrent )
                dualPathIds(parentCurrent) = dualPathIds(parentCurrent).+(newId)
                parentCurrent = path(parentCurrent)
              }

            }

          }
        }
      }

      val preTime = prePost(current)._1
      prePost(current) = (preTime,incTime())
    }

    for (j <- 0 to vertexCount-1) {
      if (visited.contains(j) == false) {
        explore(j)
      }
    }
    //println("Dual Paths" + dualPaths.mkString("\n"))
    //println(dualPathIds.mkString("\n"))
    dualPathIds

  }


}
object Solution {
  def criticalConnections(n: Int, connections: List[List[Int]]) : List[List[Int]] = {


    val graph = new Graph(n)

    for (edge <- connections) {
     graph.addEdge(edge.head,edge.tail.head)
    }


    val groupsDB = graph.dfsPrePost()

    val criticalLst = new ListBuffer[List[Int]]

    for (edge <- connections) {
      val source = edge.head
      val dest  = edge.tail.head

      if (groupsDB(source).intersect(groupsDB(dest)).isEmpty == true) {
        criticalLst.addOne(edge)
      }
    }

    criticalLst.toList

  }

  def main(args: Array[String]): Unit = {

    //println(criticalConnections(4,List(List(0,1),List(1,2),List(2,0),List(1,3))))

    val filename = "/Users/rasrivastava/FILES/1000.txt"
    val itr = Source.fromFile(filename).getLines()
    val vertextCount = itr.next().toInt
    println(vertextCount)

    val edgeStr = itr.next()

//    val first = edgeStr(1)
//    val last = edgeStr(edgeStr.length-2)

    val graph = new Graph(vertextCount)

    var first = 1
    var j = first

    val connections = new ListBuffer[List[Int]]
    while (first < edgeStr.length) {
      while (edgeStr(j) != ']') {
        j = j + 1
      }

      val subStr = edgeStr.substring(first + 1, j)

      val splitStr = subStr.split(",")
      val sourceVertex = splitStr(0).toInt
      val destVertex = splitStr(1).toInt

      connections.addOne(List(sourceVertex,destVertex))

      //println(splitStr.mkString(","))

      first = j + 2
      j = first


      //println(subStr + " first " + sourceVertex +  " " + destVertex)
    }

    println(connections.length)
    val retValue = criticalConnections(vertextCount,connections.toList)
    println(retValue.length)
    //println(criticalConnections(vertextCount,connections.toList))


//    for (line <- Source.fromFile(filename).getLines()) {
//      println(line)
//    }
  }
}