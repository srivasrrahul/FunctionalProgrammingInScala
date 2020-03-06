import scala.collection.mutable

class Point(_x : Int,_y : Int) {
  val x = _x
  val y = _y
  val dist = scala.math.sqrt(x*x + y*y)
}

object PointOrdering extends Ordering[Point] {
  override def compare(x: Point, y: Point): Int = {
    x.dist.compare(y.dist)
  }
}
object Solution {
  def kClosest(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val max_heap = new mutable.PriorityQueue[Point]()(PointOrdering)
    for (p <- points) {
      val point = new Point(p(0),p(1))
      if (max_heap.size < K){
        max_heap.addOne(point)
      }else {
        val head = max_heap.head
        if (point.dist < head.dist) {
          max_heap.dequeue()
          max_heap.addOne(point)
        }

      }
    }

    val ret_value = new mutable.ArrayBuffer[Array[Int]]()
    while (ret_value.length < K) {
      val p = max_heap.dequeue()
      ret_value.addOne(Array(p.x,p.y))
    }

    ret_value.toArray
  }

  def main(args: Array[String]): Unit = {
    val p1 = Array(3,3)
    val p2 = Array(5,-1)
    val p3 = Array(-2,4)
    val res = kClosest(Array[Array[Int]](p1,p2,p3),2)
    println(res(0).mkString(","))
    println(res(1).mkString(","))
  }
}