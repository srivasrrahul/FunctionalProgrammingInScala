import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Point(val x : Int,val y : Int)
case class Path(val char : Char,point: Point)
object Solution {
  def alphabetBoardPath(target: String): String = {
    val matrix = Array( "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "uvwxy",
      "z0000")

    val rows = matrix.length
    val cols = matrix(0).length

    def getNext(point: Point): List[Path] = {
      val px = point.x
      val py = point.y

      val next = new ListBuffer[Path]
      if (px+1 < rows && matrix(px+1)(py) != '0') {
        next.append(new Path('D',new Point(px+1,py)))
      }

      if (px-1 >= 0 && matrix(px-1)(py) != '0') {
        next.append(new Path('U',new Point(px-1,py)))
      }

      if (py+1 < cols && matrix(px)(py+1) != '0') {
        next.append(new Path('R',new Point(px,py+1)))
      }

      if (py-1 >=0 && matrix(px)(py-1) != '0') {
        next.append(new Path('L',new Point(px,py-1)))
      }

      next.toList
    }

    def getVal(point: Point) : Char = {
      //println(point)
      matrix(point.x)(point.y)
    }
    val q = new mutable.Queue[Path]()
    q.addOne(new Path('B',new Point(0,0)))

    var curr = target
    val pathTillNow = new StringBuilder
    val visited = new mutable.HashSet[Point]()

    val parent = new mutable.HashMap[Point,Path]()
    while (q.isEmpty == false && curr.isEmpty == false ) {
      val top = q.dequeue()

      val topPoint = top.point
      val path = top.char
      visited.add(topPoint)


      if (getVal(topPoint) == curr.head) {
        //found
        // println(topPoint + " " + parent)
        // println("       ")
        var currentPoint = topPoint
        val localStr = new StringBuilder
        while (parent.contains(currentPoint) == true) {
          val currentParent = parent.get(currentPoint).get
          localStr.append(currentParent.char)
          currentPoint = currentParent.point
        }

        //println(" " + localStr)
        pathTillNow.append(localStr.reverse)
        pathTillNow.append('!')
        curr = curr.tail
        visited.clear() //start new
        parent.clear()
        q.clear()
        q.addOne(new Path('B',topPoint))
        //println(curr)
      }

      for (next <- getNext(topPoint)) {
        if (visited.contains(next.point) == false) {
          parent += ((next.point,new Path(next.char,topPoint)))
          q.append(next)
        }
      }

    }


    pathTillNow.toString()
  }
}