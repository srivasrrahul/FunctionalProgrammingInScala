import scala.collection.mutable

class Node(var _value: Int) {
  var value: Int = _value
  var children: List[Node] = List()
}


object Solution {
  def findRoot(tree: List[Node]): Node = {
    val visited = new mutable.HashMap[Int,Node]()
    def explore(node : Node) : Unit = {
      visited += ((node.value,node))

      for (children <- node.children) {
        if (visited.contains(children.value) == false) {
          explore(children)
        }
      }
    }

    for (node <- tree) {
      for (child <- node.children) {
        explore(child)
      }
    }

    var root : Node = null
    for (node <- tree if root == null) {
      if (visited.contains(node.value) == false) {
        root = node
      }
    }

    root
  }
}
