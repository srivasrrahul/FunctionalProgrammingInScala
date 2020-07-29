
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    var len = 0
    var current = head
    while (current != null) {
      len = len + 1
      current = current.next
    }

    val m = len - n
    println(len + " " + m)

    if (m == 0) {
      head.next
    }else {
      //head wont change
      current = head
      var next = head.next
      var j = 0
      while (j < m-1) {
        current = current.next
        next = next.next
        j = j + 1
      }

      //println(current.x)
      if (next == null) {
        current.next = null
      }  else {
        current.next = next.next
      }


      head
    }

  }
}