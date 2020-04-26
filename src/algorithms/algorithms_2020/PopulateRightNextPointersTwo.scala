import scala.collection.mutable

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
    val q = new mutable.Queue[Node]()

    if (root != null) {
      q.addOne(root)
    }


    while (q.isEmpty == false) {
      val levelLst = q.dequeueAll(_ => true)

      for (levelNode <- levelLst) {
        if (levelNode.left != null) {
          q.addOne(levelNode.left)
        }

        if (levelNode.right != null) {
          q.addOne(levelNode.right)
        }
      }

      var first = levelLst.head
      var next = levelLst.tail

      levelLst.tail.foreach(node => {
        first.next = node
        first = node
      })
    }

    root
  }

  def main(args: Array[String]): Unit = {
    val root = new Node(1)
    val l = new Node(2)
    val r = new Node(3)
    val ll = new Node(4)
    //val lr = new Node(5)
    val rl = new Node(6)
    val rr = new Node(7)

    root.left = l
    root.right = r

    l.left = ll
    //l.right = lr

    r.left = rl
    r.right = rr

    connect(root)

    println(root.left.next.value)
    println(root.left.left.next.value)
    println(root.left.left.next.value)
  }
}