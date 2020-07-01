import scala.collection.mutable.ListBuffer

class Node(var _value: Int) {
   var value: Int = _value
   var children: List[Node] = List()
}


object Solution {
  def cloneTree(root: Node): Node = {
    def cloneNode(node : Node) : Node = {
      if (node == null) {
        null
      }else {
        val newChildLst = new ListBuffer[Node]
        for (child <- node.children) {
          val newChild = cloneNode(child)
          newChildLst.append(newChild)
        }

        val newNode = new Node(node.value)
        newNode.children = newChildLst.toList
        newNode
      }
    }

    cloneNode(root)
  }

}