import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Point(val x : Int,val y : Int)
case class Index(val p : Point)
object Solution {
  def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
    if (matrix.isEmpty) {
      0
    }else {
      val rows = matrix.length
      val cols = matrix(0).length

      def next(point: Point): List[Point] = {
        val px = point.x
        val py = point.y

        val retValue = new ListBuffer[Point]
        if (px + 1 < rows) {
          retValue.append(new Point(px + 1, py))
        }

        if (px - 1 >= 0) {
          retValue.append(new Point(px - 1, py))
        }

        if (py + 1 < cols) {
          retValue.append(new Point(px, py + 1))
        }

        if (py - 1 >= 0) {
          retValue.append(new Point(px, py - 1))
        }

        retValue.toList
      }

      def getVal(point: Point): Int = {
        matrix(point.x)(point.y)
      }

      val cache = new mutable.HashMap[Index, Int]()

      def lis(point: Point): Int = {
        val index = new Index(point)
        if (cache.contains(index)) {
          cache.get(index).get
        } else {
          val nextPoints = next(point)
          var maxPathLen = 1
          for (nextPoint <- nextPoints) {
            if (getVal(nextPoint) > getVal(point)) {
              val s = 1+lis(nextPoint)
              if (s > maxPathLen) {
                maxPathLen = s
              }
            }
          }

          cache += ((index, maxPathLen))
          maxPathLen
        }
      }

      var maxPathLen = 0
      for (j <- 0 to rows - 1) {
        for (k <- 0 to cols - 1) {
          val s = lis(new Point(j, k))
          if (s > maxPathLen) {
            maxPathLen = s
          }
        }
      }

      maxPathLen
    }
  }
}