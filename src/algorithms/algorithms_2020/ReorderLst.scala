import scala.collection.mutable

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object Solution {
  def reorderList(head: ListNode): Unit = {
    val stack = new mutable.Stack[ListNode]()

    var itr = head
    while (itr != null) {
      stack.push(itr)
      itr = itr.next
    }

    val count = stack.size
    itr = head
    //1->2->3->4->5
    //count : 0 1 2
    //count : 0 => top = 5 ,next = 2,itr = 1,itr.next = 5,top.next = 2
    //count : 1 => top = 4, itr = 2,next = 3, itr.next = 4,top.next = 3 itr = 3

    for (j <- 1 to count/2) {
      val top = stack.pop()
      val next = itr.next
      itr.next = top
      top.next = next
      itr = next
    }
    if (itr != null) {
      itr.next = null
    }


    //head
  }

  def main(args: Array[String]): Unit = {

  }
}