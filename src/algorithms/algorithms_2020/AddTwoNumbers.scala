import scala.collection.mutable

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
   var x: Int = _x
}


// class ListNode(_x: Int = 0, _next: ListNode = null) {
//   var next: ListNode = _next
//    var x: Int = _x
// }
object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val st1 = new mutable.Stack[Int]()
    val st2 = new mutable.Stack[Int]()

    var lst1 = l1
    while (lst1 != null) {
      st1.push(lst1.x)
      lst1 = lst1.next
    }

    var lst2 = l2
    while (lst2 != null) {
      st2.push(lst2.x)
      lst2 = lst2.next
    }

    var carry = 0
    var solutionList : ListNode = null
    while (st1.isEmpty == false && st2.isEmpty == false) {
      //println("test")
      val addedDigit = st1.pop() + st2.pop() + carry
      if (addedDigit >= 10) {
        val newNode = new ListNode(addedDigit % 10,solutionList)
        carry = addedDigit / 10
        solutionList = newNode
      }else {
        val newNode = new ListNode(addedDigit,solutionList)
        carry = 0
        solutionList = newNode
      }
    }

    while (st1.isEmpty == false) {
      val addedDigit = st1.pop() + carry
      if (addedDigit >= 10) {
        val newNode = new ListNode(addedDigit % 10,solutionList)
        carry = addedDigit / 10
        solutionList = newNode
      }else {
        val newNode = new ListNode(addedDigit,solutionList)
        carry = 0
        solutionList = newNode
      }
    }

    while (st2.isEmpty == false) {
      val addedDigit = st2.pop() + carry
      if (addedDigit >= 10) {
        val newNode = new ListNode(addedDigit % 10,solutionList)
        carry = addedDigit / 10
        solutionList = newNode
      }else {
        val newNode = new ListNode(addedDigit,solutionList)
        carry = 0
        solutionList = newNode
      }

    }

    if (carry != 0) {
      solutionList = new ListNode(carry,solutionList)
    }

    solutionList


  }
}