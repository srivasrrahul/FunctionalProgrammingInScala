import scala.collection.mutable
import scala.util.Random

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

class Solution(_head: ListNode) {

  /** @param head The linked list's head.
        Note that the head is guaranteed to be not null, so it contains at least one node. */
  var curr = _head
  var j = 0
  val map = new mutable.HashMap[Int,ListNode]()
  while (curr != null) {
    map += ((j,curr))
    curr = curr.next
    j = j + 1
  }

  val n = j+1
  /** Returns a random node's value. */
  def getRandom(): Int = {
    map.get(Random.between(0,n)).get.x
  }

}