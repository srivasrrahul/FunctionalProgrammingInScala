
class ListNode(var _x: Int = 0) {
   var next: ListNode = null
   var x: Int = _x
 }

//1,2,3,4,5

trait Result
case class StillFinding(val count : Int,val lastNode : ListNode) extends Result
case class Found(val res : ListNode,val lastNode : ListNode) extends Result

object Solution {
  def rotateRight1(head: ListNode, k: Int): ListNode = {
    //k > 1
    def itr(currentNode : ListNode) : Result = {
      currentNode.next match {
        case null => {
          new StillFinding(1,currentNode)
        }
        case _ => {
          val result = itr(currentNode.next)
          result match {
            case StillFinding(count,lastNode) => {
              if (k > count) {
                //println("issue")
                new StillFinding(count+1,lastNode)
              }else {
                if (k == count) {
                  //currentNode.next = null
                  val next = currentNode.next
                  currentNode.next = null
                  new Found(next,lastNode)
                }else {
                  //error
                  println("Error")
                  new StillFinding(count+1,lastNode)
                }
              }
            }
            case _ => {
              result
            }
          }
        }
      }
    }

    if (head == null || head.next == null) {
      head
    }else {
      if (k == 0) {
        head
      }else {
        val result = itr(head)
        result match {
          case Found(firstNode,lastNode) => {
            lastNode.next = head
            firstNode
          }
          case _ => {
            null
          }
        }
      }
    }

  }

  def rotateRight(head : ListNode, k : Int) : ListNode = {
    if (head == null) {
      null
    }else {
      var len = 1
      var current = head
      while (current.next != null) {
        current = current.next
        len = len + 1
      }

      val newK = k % len
      rotateRight1(head, newK)

    }

  }

  def main(args: Array[String]): Unit = {
    val head = new ListNode(1)
    head.next = new ListNode(2)
    head.next.next = new ListNode(3)
    head.next.next.next = new ListNode(4)
    head.next.next.next.next = new ListNode(5)

    val newHead = rotateRight1(head,2)

    println(newHead.x)
    println(newHead.next.x)
    println(newHead.next.next.x)
    println(newHead.next.next.next.x)
    println(newHead.next.next.next.next.x)

  }
}