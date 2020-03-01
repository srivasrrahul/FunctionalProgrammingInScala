class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}



class HeapNode(_listNode : ListNode,_index : Int) {
  val listNode : ListNode = _listNode
  val index = _index

}

object HeapNodeOrdering extends Ordering[HeapNode] {
  override def compare(l1: HeapNode, l2: HeapNode): Int = {
    l2.listNode.x.compare(l1.listNode.x)
  }
}

object Solution {
  def mergeKLists(lists: Array[ListNode]): ListNode = {
    val heap = new scala.collection.mutable.PriorityQueue[HeapNode]()(HeapNodeOrdering)

    //initialize
    var index = 0
    for (list <- lists) {
      var element = list
      while (element != null) {
        val heapNode = new HeapNode(element,index)
        heap.addOne(heapNode)
        element = element.next
      }

      index = index + 1
    }

    //println("heap_node size + " + heap.size)

    var sorted_list_root : ListNode = null
    var sorted_list : ListNode = null

    while (heap.size > 0) {
      val dequeued_heap_element = heap.dequeue()
      val smallest_node = dequeued_heap_element.listNode
      //println("Smallest Node : + " + smallest_node.x)
      val next_node_element = smallest_node.next
      smallest_node.next = null  //reset its next

      if (sorted_list_root == null) {
        sorted_list_root = smallest_node
        sorted_list = smallest_node
      }else {
        sorted_list.next = smallest_node
        sorted_list = smallest_node
      }

      val index = dequeued_heap_element.index
      if (next_node_element != null) {
        val new_heap_node = new HeapNode(next_node_element,index)
      }

    }

    sorted_list_root



  }

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode(1)
    val l4 = new ListNode(4)
    val l5 = new ListNode(5)
    val l1_1 = new ListNode(1)
    val l3 = new ListNode(3)
    val l4_4 = new ListNode(4)
    val l2 = new ListNode(2)
    val l6 = new ListNode(6)

    l1.next = l4
    l4.next = l5

    l1_1.next = l3
    l3.next = l4_4

    l2.next = l6

    val array = Array[ListNode](l1,l1_1,l2)
    var root : ListNode = mergeKLists(array)

    while (root != null) {
      print(root.x + ",")
      root = root.next
    }

  }
}