
class TreeNode(val _value :  Range) {
  val value  = _value
  var maxRange : Int = _value.max //its inclusive
  var left : TreeNode = null
  var right : TreeNode = null

  override def toString = s"TreeNode(value=$value, maxRange=$maxRange, left=$left, right=$right)"
}


class MyCalendar() {

  var root : TreeNode = null
  def ifOverlappingInterval(range : Range) : Boolean = {
    def itr(node : TreeNode) : Boolean = {
      //println(" Node " + node)
      if (node == null) {
        false //cant find an open interval
      }else {
        if (node.value.contains(range.start) ||
            node.value.contains(range.max) ||
            range.contains(node.value.start) ||
            range.contains(node.value.max)) {
          //println("true " + node.value + " " + range)
          true
        }else {
          if (node.left == null) {
            itr(node.right)
          } else {
            val maxLeft = node.left.maxRange
            if (range.start > maxLeft) {
              itr(node.right)
            }else {
              itr(node.left)
            }
          }
        }
      }
    }

    itr(root)
  }
  def addInterval(range : Range) : Unit = {
    def itr(node : TreeNode) : TreeNode = {
      if (node == null) {
        val newNode = new TreeNode(range)
        newNode
      }else {
        if (node.value.start < range.start) {
          val right = itr(node.right)
          node.right = right
          node.maxRange = math.max(node.maxRange,range.end-1)
          node
        }else {
          val left = itr(node.left)
          node.left = left
          node.maxRange = math.max(node.maxRange,range.end-1)
          node
        }
      }
    }

    if (root == null) {
      root = new TreeNode(range)
      //println(root.value + " " + root.maxRange)
    }else {
      //println("here")
      itr(root)
    }
  }
  def book(start: Int, end: Int): Boolean = {
    val range = Range(start,end)
    if (ifOverlappingInterval(range) == false) {
      addInterval(range)
      true
    }else {
      false
    }
  }

  def debug() : Unit = {
    println(root)
//    def itr(node : TreeNode) : Unit = {
//      if (node != null) {
//        println(node)
//        itr(node.left)
//        itr(node.right)
//      }
//    }

    //itr(root)
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val calendar = new MyCalendar
    println((97,100) + calendar.book(97,100).toString)
    //calendar.debug()
    println((33,51) + calendar.book(33,51).toString)

    //println((89,100) + calendar.book(89,100).toString)
    //println((83,100) + calendar.book(83,100).toString)
    println((75,92) + calendar.book(75,92).toString)

    //println((76,95)  + calendar.book(76,95).toString)
    println((19,30) + calendar.book(19,30).toString)
    calendar.debug()
    //println((53,63) + calendar.book(53,63).toString)
    //println((8,23) + calendar.book(8,23).toString)
//    println(calendar.book(18,37))
//    println(calendar.book(87,100))
//    println(calendar.book(83,100))
//    println(calendar.book(54,67))
//    println(calendar.book(35,48))
//    println(calendar.book(58,75))

    //calendar.debug()
    //println((70,89) + calendar.book(70,89).toString)
    //println(calendar.book(13,32))
    //println(calendar.book(13,32))


  }
}