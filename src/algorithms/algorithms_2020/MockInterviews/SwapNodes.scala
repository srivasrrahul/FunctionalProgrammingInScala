

// class ListNode(_x: Int = 0, _next: ListNode = null) {
//   var next: ListNode = _next
//   var x: Int = _x
// }

object Solution {
  def swapNodes(head: ListNode, k: Int): ListNode = {
    def itr(current : ListNode,count : Int) : ListNode = {
      if (count == 1) {
        current
      }else {
        itr(current.next,count-1)
      }
    }



    def itrFromEnd(current : ListNode) : (Int,ListNode) = {
      if (current == null) {
        (1,null)
      }else {
        val (counter,nextNode) = itrFromEnd(current.next)
        if (counter == k) {
          (counter+1,current)
        }else {
          if (counter > k) {
            (counter+1,nextNode)
          }else {
            (counter+1,current)
          }
        }
      }
    }

    val kthFromBegin = itr(head,k)
    val (_,kthFromEnd) = itrFromEnd(head)

//    println(kthFromBegin.x)
//    println(kthFromEnd.x)
    val temp = kthFromEnd.x
    kthFromEnd.x = kthFromBegin.x
    kthFromBegin.x = temp

    head

  }
}