import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph(val _size : Int) {
  val nodeArray = new Array[mutable.HashSet[Int]](_size)
  for (j<- 0 to _size) {
    nodeArray(j) = new mutable.HashSet[Int]()
  }

  def addNeigbours(source : Int,neigbours : Array[Int]) : Unit = {
    nodeArray(source).addAll(neigbours)
  }

  def getNeigbours(source : Int) : mutable.HashSet[Int] = {
    nodeArray(source)
  }
}
object Solution {
  def allPathsSourceTarget(g: Array[Array[Int]]): List[List[Int]] = {
    val graph = new Graph(g.length)

    for (j <- 0 to g.length-1) {
      graph.addNeigbours(j,g(j))
    }

    val retValue = new ListBuffer[List[Int]]
    def path(currentNode : Int,pathTillNow : List[Int]) : Unit = {
      if (currentNode == g.length-1) {
        retValue.append((currentNode ::pathTillNow).reverse)
      }else {
        for (neigbour <- graph.getNeigbours(currentNode)) {
          path(neigbour,currentNode :: pathTillNow)
        }
      }
    }

    retValue.toList
  }
}