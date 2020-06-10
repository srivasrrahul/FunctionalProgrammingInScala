import scala.collection.mutable

class ListNode(_x: Int = 0, _next: ListNode = null) {
 var next: ListNode = _next
   var x: Int = _x
}

object Solution {
  def isPalindrome(head: ListNode): Boolean = {
    val stack = new mutable.Stack[Int]()
    var itr = head
    while (itr != null) {
      stack.push(itr.x)
    }

    itr = head
    var palindrome = true
    while (stack.isEmpty == false && palindrome == true) {
      if (itr.x != stack.pop()) {
        palindrome = false
      }else {
        itr = itr.next
      }
    }

    palindrome
  }
}