

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def swapPairs(head: ListNode): ListNode = {
    if (head == null) {
      head
    }else {
      if (head.next == null) {
        head
      }else {
        if (head.next.next == null) {
          head.next.next = head
          val newRoot = head.next
          head.next = null
          newRoot
        }else {
          def itr(node : ListNode) : ListNode = {
            if (node == null || node.next == null) {
              node
            }else {
              val newNode = itr(node.next.next)
              val next = node.next
              next.next = node
              node.next = newNode
              next
            }
          }

          itr(head)
        }
      }
    }
  }
}