import scala.collection.mutable

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}


object Solution {
  def hasCycle(head: ListNode): Boolean = {

    def check(source : ListNode) : Boolean = {
      val visited = new mutable.HashSet[ListNode]()
      var h1 = source.next
      while (h1 != null && h1 != source && visited.contains(h1) == false) {
        visited.add(h1)
        h1 = h1.next

      }

      if (h1 == null) {
        false
      }else {
        true
      }
    }

    var current = head
    var cyclefound = false

    while (current != null && cyclefound == false) {
      cyclefound = check(current)
      current = current.next
    }


    cyclefound



  }
}