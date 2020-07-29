
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    //returns count from next
    def itr(node : ListNode,prevCount : Int) : (Int,ListNode) = {
      if (node == null) {
        (prevCount,null)
      }else {
        val currentCount = prevCount+1
        val (nextCount,nextValidNode) = itr(node.next,currentCount)
        if (nextCount - prevCount == n) {
          //delete me
          (nextCount,nextValidNode) //currentNode is deleted

        }else {
          node.next = nextValidNode
          (nextCount,node)
        }
      }
    }

    itr(head,0)._2

  }
}