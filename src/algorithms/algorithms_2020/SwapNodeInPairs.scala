

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def swapPairs(head: ListNode): ListNode = {
    //1,2,3,4
    def swap(first : ListNode,second :ListNode) : ListNode = {
      if (first == null || second == null) {
        first
      }else {
        //both are not null
        if (second.next == null) {
          second.next = first
          first.next = null
          second
        }else {
          val nextSwappedNode = swap(second.next, second.next.next)
          second.next = first
          first.next = nextSwappedNode
          second
        }
      }
    }

    swap(head,head.next)

  }
}