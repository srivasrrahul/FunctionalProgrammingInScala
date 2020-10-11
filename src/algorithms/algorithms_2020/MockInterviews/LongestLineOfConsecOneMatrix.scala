import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Point(val x : Int,val y  : Int)
trait TraversalType
case object Horizontal extends TraversalType
case object Vertical extends TraversalType
case object Diag extends TraversalType
case object AntiDiag extends TraversalType

case class PointTraversed(val point: Point,val traversalType: TraversalType)
object Solution {
  def longestLine(M: Array[Array[Int]]): Int = {

    if (M.isEmpty) {
      0
    }else {
      val rows = M.length
      val cols = M(0).length

      def getNextHorizontol(point: Point): List[Point] = {
        val lstBuffer = new ListBuffer[Point]
        val x = point.x
        val y = point.y

        if (x + 1 < rows && M(x + 1)(y) == 1) {
          lstBuffer.append(new Point(x + 1, y))
        }

        if (x - 1 >= 0 && M(x - 1)(y) == 1) {
          lstBuffer.append(new Point(x - 1, y))
        }

        lstBuffer.toList
      }

      def getNextVertical(point: Point): List[Point] = {
        val lstBuffer = new ListBuffer[Point]
        val x = point.x
        val y = point.y

        if (y + 1 < cols && M(x)(y + 1) == 1) {
          lstBuffer.append(new Point(x, y + 1))
        }

        if (y - 1 >= 0 && M(x)(y - 1) == 1) {
          lstBuffer.append(new Point(x, y - 1))
        }

        lstBuffer.toList
      }

      def getNextDiag(point: Point): List[Point] = {
        val lstBuffer = new ListBuffer[Point]
        val x = point.x
        val y = point.y

        if (x + 1 < rows && y + 1 < cols && M(x + 1)(y + 1) == 1) {
          lstBuffer.append(new Point(x + 1, y + 1))
        }

        if (x - 1 >= 0 && y - 1 >= 0 && M(x - 1)(y - 1) == 1) {
          lstBuffer.append(new Point(x - 1, y - 1))
        }

        lstBuffer.toList
      }

      def getNextAntiDiag(point: Point): List[Point] = {
        val lstBuffer = new ListBuffer[Point]
        val x = point.x
        val y = point.y

        if (x + 1 < rows && y - 1 >= 0 && M(x + 1)(y - 1) == 1) {
          lstBuffer.append(new Point(x + 1, y - 1))
        }

        if (x - 1 >= 0 && y + 1 < cols && M(x - 1)(y + 1) == 1) {
          lstBuffer.append(new Point(x - 1, y + 1))
        }

        lstBuffer.toList
      }


      val visited = new mutable.HashSet[PointTraversed]()
      def bfs(point: Point, traversalType: TraversalType,nextFun: (Point => List[Point])): Int = {


        val pathLen = new mutable.HashMap[Point, Int]()

        val q = new mutable.Queue[Point]()
        q.addOne(point)

        pathLen += ((point, 1))

        while (q.isEmpty == false) {
          val top = q.dequeue()
          val pointTraversed = new PointTraversed(top,traversalType)
          visited.add(pointTraversed)
          val nexts = nextFun(top)

          for (next <- nexts) {
            if (visited.contains(new PointTraversed(next,traversalType)) == false) {
              q.append(next)
              pathLen += ((next, pathLen.get(top).get + 1))
            }
          }

        }

        var maxPathLen = 1
        for ((_, len) <- pathLen) {
          maxPathLen = math.max(maxPathLen, len)
        }

        maxPathLen
      }

      var maxPathLen = 0
      for (j <- 0 to rows - 1) {
        for (k <- 0 to cols - 1) {
          val point = new Point(j, k)
          if (M(j)(k) == 1) {
            maxPathLen = math.max(maxPathLen, bfs(point, Horizontal,getNextHorizontol))
            maxPathLen = math.max(maxPathLen, bfs(point, Vertical,getNextVertical))
            maxPathLen = math.max(maxPathLen, bfs(point, Diag,getNextDiag))
            maxPathLen = math.max(maxPathLen, bfs(point, AntiDiag,getNextAntiDiag))
          }
        }
      }

      maxPathLen
    }
  }
}