

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    if (l1 == null || l2 == null) {
      if (l1 == null) {
        l2
      }else {
        l1
      }
    }else {
      var t1 = l1
      var t2 = l2

      var head: ListNode = null
      var current: ListNode = null

      while (t1 != null && t2 != null) {
        if (t1.x <= t2.x) {
          if (head == null) {
            head = t1
            current = t1
          } else {
            current.next = t1
            current = current.next
          }

          t1 = t1.next
          current.next = null

        } else {
          if (head == null) {
            head = t2
            current = t2
          } else {
            current.next = t2
            current = current.next
          }
          t2 = t2.next
          current.next = null
        }
      }


      while (t1 != null) {
        current.next = t1
        t1 = t1.next
        current = current.next
        current.next = null
      }

      while (t2 != null) {
        current.next = t2
        t2 = t2.next
        current = current.next
        current.next = null
      }

      head
    }
  }
}