

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def deleteDuplicates(head: ListNode): ListNode = {
    def remove(current : ListNode) : ListNode = {
      if (current == null) {
        null
      }else {
        var t = current.next
        var similar = 0
        while (t != null && t.x == current.x) {
          t = t.next
          similar = similar+1
        }

        if (similar > 0) {
          remove(t)
        }else {
          val next = remove(t)
          current.next = next
          current
        }
      }
    }

    remove(head)
  }
}