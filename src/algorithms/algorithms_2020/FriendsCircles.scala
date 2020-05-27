import scala.collection.mutable

object Solution {
  def findCircleNum(friendStatus: Array[Array[Int]]): Int = {
    def bfs(sourceIndex : Int) : Set[Int] = {
      val q = new mutable.Queue[Int]()
      val visited = new mutable.HashSet[Int]()

      q.addOne(sourceIndex)

      while (q.isEmpty == false) {
        val top = q.dequeue()
        visited.add(top)

        for (j <- 0 to friendStatus.length-1) {
          val neigbourStat = friendStatus(top)(j)
          if (neigbourStat == 1 && visited.contains(j) == false) {
            q.addOne(j)
          }
        }
      }

      visited.toSet

    }

    var unexploredSet = new mutable.HashSet[Int]()
    for (j <- 0 to friendStatus.length-1) {
      unexploredSet.add(j)
    }

    var count = 0
    while (unexploredSet.isEmpty == false) {
      val visitedSet = bfs(unexploredSet.head)
      //println(visitedSet + " for " + unexploredSet.head)
      unexploredSet = unexploredSet.diff(visitedSet)
      //println(unexploredSet)
      count = count + 1
    }

    count

  }

  def main(args: Array[String]): Unit = {
    var arr = Array(Array(1,1,0),Array(1,1,1),Array(0,1,1))
    println(findCircleNum(arr))
  }
}