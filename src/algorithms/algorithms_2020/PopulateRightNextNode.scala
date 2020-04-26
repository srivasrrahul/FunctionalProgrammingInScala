
class Node(var _value: Int) {
  var value: Int = _value
  var left: Node = null
  var right: Node = null
  var next: Node = null
}

object Solution {

  def isLeaf(node : Node) : Boolean = {
    node.left == null && node.right == null
  }
  def connect(root: Node): Node = {
    def explore(node : Node) : (List[Node],List[Node]) = {
      if (isLeaf(node.left) && isLeaf(node.right)) {
        node.left.next = node.right
        (List(node.left),List(node.right))
      }else {
        val (lleftMost,lrightMost) = explore(node.left)
        val (rleftMost,rrightMost) = explore(node.right)
        node.left.next = node.right

        var lrlist = lrightMost
        val rllist = rleftMost

        for ((x,y) <- (lrightMost zip rllist)) {
          x.next = y
        }

        (node.left :: lleftMost,node.right :: rrightMost)
      }
    }

    if (root == null) {
      root
    }else {
      if (isLeaf(root)) {
        root
      }else {
        explore(root)
        root
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val root = new Node(1)
    val l = new Node(2)
    val r = new Node(3)
    val ll = new Node(4)
    val lr = new Node(5)
    val rl = new Node(6)
    val rr = new Node(7)

    root.left = l
    root.right = r

    l.left = ll
    l.right = lr

    r.left = rl
    r.right = rr

    connect(root)

    println(root.left.next.value)
    println(root.left.left.next.value)
    println(root.left.right.next.value)
  }
}