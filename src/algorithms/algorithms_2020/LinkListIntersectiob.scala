import scala.collection.mutable

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
 }

object Solution {
  def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
    val stack1 = new mutable.Stack[ListNode]()
    var itr1 = headA

    while (itr1 != null) {
      stack1.push(itr1)
      itr1 = itr1.next
    }

    val stack2 = new mutable.Stack[ListNode]()
    var itr2 = headB

    while (itr2 != null) {
      stack2.push(itr2)
      itr2 = itr2.next
    }

    //Pop till they differ
    var pointOfIntersection : ListNode = null
    var diffFound = false
    while (stack1.isEmpty == false && stack2.isEmpty == false && diffFound == false) {
      val s1 = stack1.pop()
      val s2 = stack2.pop()

      if (s1 == s2) {
        pointOfIntersection = s1
      }else {
        diffFound = true
      }
    }

    pointOfIntersection


  }
}