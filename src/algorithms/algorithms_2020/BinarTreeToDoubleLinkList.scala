class Node(var _value: Int) {
   var value: Int = _value
   var left: Node = null
   var right: Node = null
}

object Solution {
  def isLeaf(node : Node) : Boolean = {
    node.left == null && node.right == null
  }
  def treeToDoublyList(root: Node): Node = {
    def explore(node : Node) : (Node,Node) = {
      if (node == null) {
        (null,null)
      }else {
        if (isLeaf(node)) {
          (node, node)
        } else {
          val (lMin, lMax) = explore(node.left)
          val (rMin, rMax) = explore(node.right)

          if (lMax != null) {
            lMax.right = node
            node.left = lMax
          }else {
            node.left = null
          }

          if (rMin != null) {
            rMin.left = node
            node.right = rMin
          }else {
            node.right = null
          }

          var retMin = lMin

          if (lMin == null) {
            retMin = lMax
          }

          if (lMax == null) {
            retMin = node
          }

          var retMax = node
          if (rMin != null) {
            retMax = rMin
          }

          if (rMax != null) {
            retMax = rMax
          }

          (retMin,retMax)
        }
      }
    }

    val (head,end) = explore(root)
    if (head != null && end != null) {
      end.right = head
      head.left = end
    }

    head
  }

  def main(args: Array[String]): Unit = {
    val root = new Node(10)
    val l = new Node(5)
    val r = new Node(20)

    root.left = l
    root.right = r

    val ll = new Node(3)
    val lr = new Node(7)
    l.left = ll
    l.right = lr

    var head = treeToDoublyList(root)
    var current = head
    while (head != null) {
      println(head.value)
      current = head
      head = head.right
    }

    println(" ")
    while (current != null) {
      println(current.value)
      current = current.left
    }
  }
}