import scala.collection.mutable

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution {
  def create_tree(list_index : mutable.HashMap[Int,ListNode],begin : Int,end : Int) : TreeNode = {
    //println("")
    val range = end-begin+1
    println("range is " + begin + " " + end)
    range match {
      case 1 => {
        new TreeNode(list_index.get(begin).get.x)
      }
      case 2 => {
        val n1 = new TreeNode(list_index.get(begin).get.x)
        val n2 = new TreeNode(list_index.get(end).get.x)
        n2.left = n1
        n2
      }
      case 3 => {
        val n1 = new TreeNode(list_index.get(begin).get.x)
        val n2 = new TreeNode(list_index.get(begin+1).get.x)
        val n3 = new TreeNode(list_index.get(end).get.x)
        n2.left = n1
        n2.right = n3
        n2
      }
      case _ => {
        val mid = begin + (end-begin)/2
        val r = new TreeNode(list_index.get(mid).get.x)
        val left = create_tree(list_index,begin,mid-1)
        val right = create_tree(list_index,mid+1,end)
        r.left = left
        r.right = right
        r
      }
    }
  }
  def sortedListToBST(head: ListNode): TreeNode = {
    val list_index = new mutable.HashMap[Int,ListNode]()

    var index = 0
    var list_node = head
    while (list_node != null) {
      list_index += ((index,list_node))
      index += 1
      list_node = list_node.next
    }

    if (index == 0) {
      null
    }else {
      val root = create_tree(list_index,0,index-1)
      root
    }


  }

  def walk(node : TreeNode): Unit = {
    node match {
      case null => {

      }
      case _ => {
        walk(node.left)
        print(node.value)
        walk(node.right)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val l1 = new ListNode(1)
    val l2 = new ListNode(2)
    val l3 = new ListNode(3)
    val l4 = new ListNode(4)

    l1.next = l2
    l2.next = l3
    l3.next = l4

    val root = sortedListToBST(l1)
    walk(root)

  }
}