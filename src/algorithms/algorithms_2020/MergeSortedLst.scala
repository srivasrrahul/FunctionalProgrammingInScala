
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution {
//  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
//    var firstNode :Option[ListNode] = None
//
//    var newItr : Option[ListNode] = None
//
//    var itr1 = l1
//    var itr2 = l2
//    while (itr1 != null && itr2 != null) {
//      newItr match {
//        case None => {
//          if (itr1.x <= itr2.x) {
//            firstNode = Some(new ListNode(itr1.x))
//            itr1 = itr1.next
//          }else {
//            firstNode = Some(new ListNode(itr2.x))
//            itr2 = itr2.next
//          }
//
//          newItr = firstNode
//        }
//        case Some(itrRef : ListNode) => {
//          if (itr1.x <= itr2.x) {
//            val newNode = new ListNode(itr1.x)
//            itrRef.next = newNode
//            newItr = Some(newNode)
//            itr1 = itr1.next
//          } else {
//            val newNode = new ListNode(itr2.x)
//            itrRef.next = newNode
//            newItr = Some(newNode)
//            itr2 = itr2.next
//          }
//        }
//      }
//    }
//
//    while (itr1 != null) {
//      newItr match {
//        case None => {
//          val newNode = new ListNode(itr1.x)
//          firstNode = Some(newNode)
//          newItr = firstNode
//
//          itr1 = itr1.next
//        }
//        case Some(itrRef) => {
//          val newNode = new ListNode(itr1.x)
//          itrRef.next = newNode
//          newItr = Some(newNode)
//          itr1 = itr1.next
//        }
//      }
//
//    }
//
//
//
//    while (itr2 != null) {
//      newItr match {
//        case None => {
//          val newNode = new ListNode(itr2.x)
//          firstNode = Some(newNode)
//          newItr = firstNode
//
//          itr1 = itr1.next
//        }
//        case Some(itrRef) => {
//          val newNode = new ListNode(itr2.x)
//          itrRef.next = newNode
//          newItr = Some(newNode)
//
//          itr2 = itr2.next
//        }
//      }
//
//    }
//
//    firstNode match {
//      case None => null
//
//      case Some(start) => start
//    }
//  }

  def mergeTwoLists(l1 : ListNode,l2 : ListNode) : ListNode = {
    def merge(l1Itr : ListNode,l2Itr: ListNode) : ListNode = {
      (l1Itr,l2Itr) match {
        case (null,null) => null
        case (_,null) => {
          val newNode = new ListNode(l1Itr.x)
          newNode.next = merge(l1Itr.next,l2Itr)
          newNode
        }
        case (null,_) => {
          val newNode = new ListNode(l2Itr.x)
          newNode.next = merge(l1Itr,l2Itr.next)
          newNode
        }
        case (_,_) => {
          if (l1Itr.x <= l2Itr.x) {
            val newNode = new ListNode(l1Itr.x)
            newNode.next = merge(l1Itr.next,l2Itr)
            newNode
          }else {
            val newNode = new ListNode(l2Itr.x)
            newNode.next = merge(l1Itr,l2Itr.next)
            newNode
          }
        }
      }
    }

    merge(l1,l2)
  }

  def main(args: Array[String]): Unit = {
    val l1a = new ListNode(1)
    val l1b = new ListNode(10)
    val l1c = new ListNode(20)

    val l2a = new ListNode(5)
    val l2b = new ListNode(15)
//    val l2c = new ListNode(30)
//    val l2d = new ListNode(40)

    l1a.next = l1b
    l1b.next = l1c

    l2a.next = l2b
//    l2b.next = l2c
//    l2c.next = l2d

    val lmerged = mergeTwoLists(l1a,l2a)

    var itr = lmerged
    while (itr != null) {
      println(itr.x)
      itr = itr.next
    }
  }
}