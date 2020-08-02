import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Tuple(val x : Int,val y : Int)
class Graph(val arr : Array[Array[Int]]) {
  val maxRows = arr.length-1
  val maxCols = arr(0).length-1
  def next(source : Tuple) : List[Tuple] = {
    //give next which has gold
    val next = new ListBuffer[Tuple]
    val x = source.x
    val y = source.y
    if (x-1 >= 0 && arr(x-1)(y) != 0) {
      next.append(new Tuple(x-1,y))
    }

    if (source.x+1 <= maxRows && arr(source.x+1)(source.y) != 0) {
      next.append(new Tuple(x+1,y))
    }

    if (source.y-1 >= 0 && arr(source.x)(source.y-1) != 0) {
      next.append(new Tuple(source.x,source.y-1))
    }

    if (source.y+1 <= maxCols && arr(source.x)(source.y+1) != 0) {
      next.append(new Tuple(x,y+1))
    }

    next.toList
  }

  def getValue(source : Tuple) : Int = {
    arr(source.x)(source.y)
  }
}
case class Key(val current : Tuple,visited : Set[Tuple])
object Solution {
  def getMaximumGold(grid: Array[Array[Int]]): Int = {
    val graph = new Graph(grid)
    val cache = new mutable.HashMap[Key,Int]()
    def dfs(source : Tuple) : Int = {

      var maxGoldPossible = Int.MinValue
      def explore(source : Tuple,goldCollectedTillNow : Int,visited : Set[Tuple]) : Int = {
        val next = graph.next(source).toSet
        //println("For soruce " + source + " " + next + " " + visited)
        val diff = next.diff(visited)
        if (diff.isEmpty) {
          val totalGold = goldCollectedTillNow+graph.getValue(source)
          if (totalGold > maxGoldPossible) {
            maxGoldPossible = totalGold
          }
          totalGold
        }else {
          val key = new Key(source,visited)
          if (cache.contains(key)) {
            cache.get(key).get
          }else {
            var maxGoldFoundLocal = Int.MinValue
            for (validNext <- diff) {
              val goldFound = explore(validNext, goldCollectedTillNow + graph.getValue(source), visited.+(source))
              if (goldFound > maxGoldFoundLocal) {
                maxGoldFoundLocal = goldFound
              }



            }

            if (maxGoldFoundLocal != Int.MinValue) {
              cache += ((key,maxGoldFoundLocal))
            }

            maxGoldFoundLocal
          }
        }
      }

      explore(source,0,Set())

      maxGoldPossible
    }

    var maxGoldCollected = Int.MinValue
    for (j <- 0 to graph.maxRows) {
      for (k <- 0 to graph.maxCols) {
        val source = new Tuple(j,k)
        if (graph.getValue(source) > 0) {
          val goldCollected = dfs(source)
          //println(source + " " + goldCollected)
          if (goldCollected > maxGoldCollected) {
            maxGoldCollected = goldCollected
          }
        }
      }
    }

    maxGoldCollected
  }
}