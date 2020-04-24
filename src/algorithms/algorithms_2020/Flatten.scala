import scala.collection.mutable.ListBuffer

class Node(var _value: Int) {
   var value: Int = _value
   var prev: Node = null
   var next: Node = null
   var child: Node = null
}
object Solution {
  def flatten(head: Node): Node = {
    val lstBuffer = new ListBuffer[Node]

    //first and last
    def explore(node : Node) : (Node,Node) = {
      if (node != null) {
        lstBuffer.append(node)
        if (node.child != null) {
          val next = node.next

          val (accruedFirstNode,accruedLastNode) = explore(node.child)


          node.next = accruedFirstNode
          accruedFirstNode.prev = node
          node.child = null

          accruedLastNode.next = next

          if (next != null) {
            next.prev = accruedLastNode
          }

          var lastNode = accruedLastNode
          if (accruedLastNode.next != null) {
            val (t1,t2) = explore(accruedLastNode.next)
            if (t2 != null) {
              lastNode = t2
            }
          }

          (node,lastNode)


        }else {
          if (node.next != null) {
            val (_,lastNode) = explore(node.next)
            (node,lastNode)
          }else {
            (node,node)
          }

        }
      }else {
        (null,null)
      }
    }

    explore(head)._1
  }

  def main(args: Array[String]): Unit = {
    val h1 = new Node(1)
    val h2 = new Node(2)
    val h3 = new Node(3)
    val h4 = new Node(4)
    val h5 = new Node(5)

    h1.next = h2
    h2.prev = h1

    h2.next = h5
    h5.prev = h2

    h2.child = h3

    h3.next = h4
    h4.prev = h3

    val res = flatten(h1)

    println(res.value)
    println(res.next.value)
    println(res.next.next.value)
    println(res.next.next.next.value)
    println(res.next.next.next.next.value)




  }
}