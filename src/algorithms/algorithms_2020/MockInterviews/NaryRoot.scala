import scala.collection.mutable
import scala.collection.mutable.ListBuffer

 class Node(var _value: Int) {
   var value: Int = _value
   var children: List[Node] = List()
 }


object Solution {
  def findRoot(tree: List[Node]): Node = {
    val parent = new mutable.HashSet[Int]()

    def itr(node : Node) : Unit = {
      if (node == null) {

      }else {
        for (c <- node.children) {
          if (parent.contains(c.value) == false) {
            parent.add(c.value)
            itr(c)
          }
        }
      }
    }

    for (t <- tree) {
      itr(t)
    }

    var root : Node = null
    for (t <- tree if root == null) {
      if (parent.contains(t.value) == false) {
        root = t
      }
    }

    root

  }
}
