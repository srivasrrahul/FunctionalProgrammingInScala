
class Node(var _value: Int) {
  var value: Int = _value
  var neighbors: List[Node] = List()
}


class Graph {
  val nodeMap = new scala.collection.mutable.HashMap[Int,Node]
  def addNode(nodeId : Int) : Unit = {
    nodeMap += ((nodeId,new Node(nodeId)))
  }

  def checkNodeExists(nodeId : Int) : Boolean = {
    nodeMap.contains(nodeId)
  }

  def addNeigbour(fromNodeInOtherGraph : Node, toNodeInOtherGraph : Node) : Unit = {
    val nodeId = fromNodeInOtherGraph.value
    val fromNodeInNewGraph = nodeMap.get(nodeId).get

    val toNodeInNewGraph = nodeMap.get(toNodeInOtherGraph.value).get
    fromNodeInNewGraph.neighbors = toNodeInNewGraph :: fromNodeInNewGraph.neighbors
  }
}
object Solution {
  def cloneGraph(node: Node): Node = {
    if (node == null) {
      null
    }else {
      val newGraph = new Graph

      //visit all nodes and edges
      val visited = new scala.collection.mutable.HashMap[Int,Node] //old graph
      val q = new scala.collection.mutable.Queue[Node] //old graph

      q.addOne(node)

      while (q.isEmpty == false) {
        val top = q.dequeue()
        visited += ((top.value,top))

        if (newGraph.nodeMap.contains(top.value) == false) {
          newGraph.addNode(top.value)
        }

        for (neigbour <- top.neighbors) {
          if (visited.contains(neigbour.value) == false) {
            q.addOne(neigbour)
          }
        }
      }

      //all nodes are added in new graph
      //all nodes are added in visited

      //walk all nodes and add neigbours accordingly
      visited.foreachEntry((nodeId,oldNode) => {
        for (neigbour <- oldNode.neighbors) {
          newGraph.addNeigbour(oldNode,neigbour)
        }
      })

      //println(newGraph.nodeMap)
      newGraph.nodeMap.get(node.value).get
    }


  }
}