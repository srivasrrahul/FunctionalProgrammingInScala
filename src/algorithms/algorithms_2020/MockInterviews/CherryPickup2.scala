import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Pos(val x : Int,val y : Int)
case class State(val r1 : Pos,val r2 : Pos)

object Solution {
  def cherryPickup(grid: Array[Array[Int]]): Int = {
    val rowSize = grid.length
    val colSize = grid(0).length
    def next(pos : Pos): List[Pos] = {
      val x = pos.x
      val y = pos.y
      val res = new ListBuffer[Pos]

      if (x+1 < rowSize) {
        res.append(new Pos(x+1,y))
        if (y+1 < colSize) {
          res.append(new Pos(x+1,y+1))
        }

        if (y-1 >= 0) {
          res.append(new Pos(x+1,y-1))
        }
      }

      res.toList
    }

    val cache = new mutable.HashMap[State,Int]()
    def itr(rob1 : Pos,rob2 : Pos) : Int = {
      if (rob1.x == rowSize) {
        if (rob1.y == rob2.y) {
          grid(rob1.x)(rob1.y)
        }else {
          grid(rob1.x)(rob1.y) + grid(rob2.x)(rob2.y)
        }
      }else {
        val state = new State(rob1,rob2)
        if (cache.contains(state)) {
          cache.get(state).get
        }else {
          var collected = 0
          if (rob1.y == rob2.y) {
            collected = grid(rob1.x)(rob1.y)
          } else {
            collected = grid(rob1.x)(rob1.y) + grid(rob2.x)(rob2.y)
          }

          val r1Next = next(rob1)
          val r2Next = next(rob2)

          var maxNextCollected = 0
          for (r1 <- r1Next) {
            for (r2 <- r2Next) {
              val nextCollected = itr(r1, r2)
              if (nextCollected > maxNextCollected) {
                maxNextCollected = nextCollected
              }
            }
          }

          cache += ((state,collected+maxNextCollected))
          collected + maxNextCollected
        }
      }
    }

    itr(new Pos(0,0),new Pos(0,colSize-1))
  }
}