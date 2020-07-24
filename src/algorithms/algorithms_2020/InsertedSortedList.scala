

class Node(var _value: Int) {
  var value: Int = _value
  var next: Node = null
}


object Solution {
  def insert(head: Node, insertVal: Int): Node = {
    if (head == null) {
      val node = new Node(insertVal)
      node.next = node
      node
    }else {
      //if there is a single node
      if (head.next == head) {
        val node = new Node(insertVal)
        node.next = head
        head.next = node
        head
      }else {
        //At least two now
        var current = head
        var next = head.next

        while (false == ((next == head) || (current.value <= insertVal && insertVal <= next.value) ||
          (current.value > insertVal && insertVal < next.value && current.value > next.value) ||
          (current.value <= insertVal && insertVal >= next.value && current.value >  next.value)
          )) {

          current = current.next
          next = next.next
        }

        //println((next == head) + " " + current.value)

        val newNode = new Node(insertVal)
        current.next = newNode
        newNode.next = next

        head

      }
    }
  }
}