import scala.collection.mutable

class Node(var _value: Int) {
  var value: Int = _value
  var left: Node = null
  var right: Node = null
  var next: Node = null
}


object Solution {
  def connect(root: Node): Node = {
    val q = new mutable.Queue[Node]()
    if (root != null) {
      q.append(root)
    }

    while (q.isEmpty == false) {
      val lst = q.dequeueAll(_ => true)

      var first = lst.head
      var next = lst.tail

      while (next.isEmpty == false) {
        first.next = next.head
        first = next.head
        next = next.tail;
      }

      for (l <- lst) {
        if (l.left != null) {
          q.append(l.left)
        }

        if (l.right != null) {
          q.append(l.right)
        }
      }
    }

    root
  }
}