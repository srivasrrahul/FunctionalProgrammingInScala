import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
object Solution {
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val M = obstacleGrid.length - 1
    if (M == 0) {
      0
    } else {

      val N = obstacleGrid(0).length - 1

      def next(current: Index): List[Index] = {
        val lstBuffer = new ListBuffer[Index]
        if (current.x < M) {
          val down = new Index(current.x + 1, current.y)
          if (obstacleGrid(down.x)(down.y) == 0) {
            lstBuffer.append(down)
          }
        }

        if (current.y < N) {
          val right = new Index(current.x, current.y + 1)
          if (obstacleGrid(right.x)(right.y) == 0) {
            lstBuffer.append(right)
          }
        }

        lstBuffer.toList
      }

      val cache = new mutable.HashMap[Index,Option[Int]]()
      def itr(current: Index): Option[Int] = {
        (current.x, current.y) match {
          case (M, N) => {
            Some(1)
          }
          case _ => {
            if (cache.contains(current)) {
              cache.get(current).get
            }else {
              val nextLst = next(current)
              var count = 0
              for (nextPath <- nextLst) {
                itr(nextPath) match {
                  case Some(pathCount) => {
                    count = count + pathCount
                  }
                  case None => {

                  }
                }
              }

              if (count == 0) {
                cache += ((current, None))
                None
              } else {
                cache += ((current, Some(count)))
                Some(count)
              }
            }
          }
        }
      }

      val first = new Index(0, 0)
      if (obstacleGrid(first.x)(first.y) == 1) {
        0
      } else {
        itr(first) match {
          case None => 0
          case Some(count) => count
        }
      }

    }
  }

  def uniquePathsWithObstaclesDP(obstacleGrid: Array[Array[Int]]) : Int = {
    val M = obstacleGrid.length-1
    if (M < 0) {
      0
    }else {
      val N = obstacleGrid(0).length-1
      val matrix = Array.ofDim[Int](M+1,N+1)

      if (obstacleGrid(M)(N) == 1) {
        0
      }else {
        matrix(M)(N) = 1

        def validIndex(index : Index) : Boolean = {
          if (index.x >= 0 && index.y >= 0) {
            obstacleGrid(index.x)(index.y)  == 0
          }else {
            false
          }
        }


        def itr(current : Index) : Unit = {
          val left = new Index(current.x-1, current.y)
          val up = new Index(current.x,current.y-1)

          if (validIndex(left)) {
            matrix(left.x)(left.y) = matrix(left.x)(left.y) + 1
            itr(left)
          }

          if (validIndex(up)) {
            matrix(up.x)(up.y) = matrix(up.x)(up.y) + 1
            itr(up)
          }

        }

        itr(new Index(M,N))

        matrix(0)(0)


      }
    }

  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(0,0,0),Array(0,1,0),Array(0,0,0))
    println(uniquePathsWithObstacles(arr))
  }
}