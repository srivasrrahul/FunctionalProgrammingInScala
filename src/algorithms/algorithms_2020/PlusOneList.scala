
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def plusOne(head: ListNode): ListNode = {
    def itr(current : ListNode) : (ListNode,Int) = {
      current.next match {
        case null => {
          val newDigit = current.x + 1
          if (newDigit >= 10) {
            current.x = newDigit % 10
            (current,newDigit / 10)
          }else {
            current.x = newDigit
            (current,0)
          }
        }
        case _ => {
          val (next,carry) = itr(current.next)
          val newDigit = current.x + carry
          if (newDigit >= 10) {
            current.x = newDigit % 10
            (current,newDigit / 10)
          }else {
            current.x = newDigit
            (current,0)
          }
        }
      }
    }

    if (head == null) {
      null
    }else {
      val (nextNode, carry) = itr(head)
      if (carry > 0) {
        new ListNode(carry, nextNode)
      } else {
        nextNode
      }
    }
  }
}