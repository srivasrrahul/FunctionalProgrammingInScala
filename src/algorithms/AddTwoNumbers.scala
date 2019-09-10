import util.control.Breaks._
class ListNode(var _x : Int = 0) {
  var next : ListNode = null
  var x : Int = _x

//  override def toString: String = {
//    val curr = new String(x.toString)
//    if (next != null) {
//      val p = new String(next.toString)
//      val newStr = curr + " -> " + p
//      newStr
//    }else {
//      curr
//    }
//
//
//  }
}

object Soluton {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var itr1 = l1
    var itr2 = l2
    var carry = 0
    var res : ListNode = null
    var resItr = res

    breakable {
      while (true) {
        //println("Loop")
        var current : Int = 0
        if (itr1 == null && itr2 == null) {
          if (carry != 0) {
            resItr.next = new ListNode(carry)
          }
          break
        }




        if (itr1 != null) {
          current += itr1.x
          itr1 = itr1.next
        }

        if (itr2 != null) {
          current += itr2.x
          itr2 = itr2.next
        }

        current = current + carry
        println(current)
        if (current > 9) {
          val lastDig = current % 10
          carry = (current - lastDig) / 10
          current = lastDig
        }else {
          carry = 0
        }


        if (res == null) {
          val newNode = new ListNode(current)
          res = newNode
          resItr = res
        }else {
          resItr.next = new ListNode(current)
          resItr = resItr.next

        }

      }
    }

    res
  }

//  def main(args: Array[String]): Unit = {
//    val a1 = new ListNode(9)
//    val a2 = new ListNode(9)
//    val a3 = new ListNode(9)
//    a1.next = a2
//    a2.next = a3
//
//    val b1 = new ListNode(9)
//    val b2 = new ListNode(9)
//    val b3 = new ListNode(9)
//    val b4 = new ListNode(9)
//    b1.next = b2
//    b2.next = b3
//    b3.next = b4
//
//    println(addTwoNumbers(a1,b1))
//  }
}