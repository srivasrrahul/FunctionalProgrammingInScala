import scala.collection.mutable

case class Node(val index : Int) {
  val next = new mutable.HashSet[Int]()
}
object Solution {
  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val graph = new Array[Node](rooms.length)
    var index = 0
    for (room <- rooms) {
      val node = new Node(index)
      for (v <- room) {
        node.next.add(v)
      }

      graph(index) = node
      index = index+1
    }

    val visited = new mutable.HashSet[Int]()
    val q = new mutable.Queue[Int]()
    q.addOne(0)

    while (q.isEmpty == false) {
      val top = q.dequeue()
      visited.add(top)

      for (next <- graph(top).next) {
        if (visited.contains(next) == false) {
          q.addOne(next)
        }
      }
    }

    visited.size == rooms.size
  }
}