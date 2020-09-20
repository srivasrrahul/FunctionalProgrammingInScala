import scala.collection.mutable
import scala.util.control.Breaks._
case class Node(val x : Int,val y : Int)
case class PQNode(val dist : Int,val node : Node)

object PQOrdering extends Ordering[PQNode] {
  override def compare(x: PQNode, y: PQNode): Int = {
    y.dist.compare(x.dist)
  }
}
object Solution {
  def nextNightMove(source : Node) : List[Node] = {
    val rightUp = new Node(source.x+2,source.y+1)
    val rightDown = new Node(source.x+2,source.y-1)

    val leftUp = new Node(source.x-2,source.y+1)
    val leftDown = new Node(source.x-2,source.y-1)

    val downLeft = new Node(source.x-1,source.y-2)
    val downRight = new Node(source.x+1,source.y-2)

    val upLeft = new Node(source.x-1,source.y+2)
    val upRight = new Node(source.x+1,source.y+2)

    List(rightUp,rightDown,leftUp,leftDown,downLeft,downRight,upLeft,upRight)
  }
  def minKnightMoves(x: Int, y: Int): Int = {
    val pq = mutable.PriorityQueue.empty[PQNode](PQOrdering)


    pq.addOne(new PQNode(0,new Node(0,0)))
    val target = new Node(x,y)
    var maxDist = Int.MaxValue
    val visited = new mutable.HashSet[Node]()

    while (maxDist == Int.MaxValue && pq.size > 0) {
      val top = pq.dequeue()
      visited.add(top.node)
      //println("distabce " + top.dist)
      if (top.node == target) {
        //println("Natcged ")
        maxDist = top.dist
      } else {
        for (next <- nextNightMove(top.node)) {
          if (visited.contains(next) == false) {
            pq.addOne((new PQNode(top.dist + 1, next)))
            visited.add(next)
          }
        }

        //println("PQ Node is " + pq)
        //println("        ")
      }
    }

    //println("Max Dist is " + maxDist)
    //Npw maxDist is found. Do a BFS and discard those nodes whopse distance is greater
    pq.clear()
    visited.clear()
    val q = new mutable.Queue[PQNode]()
    q.addOne(new PQNode(0,new Node(0,0)))

    var bfsFoundDist = Int.MaxValue
    while (q.isEmpty == false && bfsFoundDist == Int.MaxValue) {
      val top = q.dequeue()
      if (top.node == target) {
        bfsFoundDist = top.dist
      }else {
        visited.add(top.node)
        val newDist = top.dist+1
        if (newDist <= maxDist) {
          for (next <- nextNightMove(top.node)) {
            if (visited.contains(next) == false) {
              q.append(new PQNode(top.dist+1,next))
              visited.add(next)
            }
          }
        }
      }
    }

    bfsFoundDist
  }

  def main(args: Array[String]): Unit = {
    println(minKnightMoves(20,20))
  }
}