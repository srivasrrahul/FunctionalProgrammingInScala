import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
case class State(val r1 : Index,val r2 : Index)
object Solution {
  def cherryPickup(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length
    def collect(state: State) : Int = {
      if (state.r1 == state.r2) {
        grid(state.r1.x)(state.r1.y)
      }else {
        grid(state.r1.x)(state.r1.y) + grid(state.r2.x)(state.r2.y)
      }
    }
    def isFinal(state : State) : Boolean = {
      state.r1.x == rows-1 && state.r2.x == rows-1
    }

    def getNextState(state : State) : List[State] = {
      def nextIndex(index : Index) : List[Index] = {
        val lstBuffer = new ListBuffer[Index]
        if ((index.x)+1 < rows) {
          lstBuffer.append(new Index((index.x)+1,index.y))

          if ((index.y)-1 >= 0) {
            lstBuffer.append(new Index((index.x)+1,(index.y)-1))
          }

          if ((index.y)+1 < cols) {
            lstBuffer.append(new Index((index.x)+1,(index.y)+1))
          }
        }

        lstBuffer.toList


      }

      val nextStates = new ListBuffer[State]
      val nextR1Indexes = nextIndex(state.r1)
      val nextR2Indexes = nextIndex(state.r2)

      val cross = nextR1Indexes.flatMap(r1 => nextR2Indexes.map(r2 => new State(r1,r2)))
      cross
    }


    val cache = new mutable.HashMap[State,Int]()
    def itr(state : State) : Int = {
      if (cache.contains(state) == false) {

        //
        if (isFinal(state)) {
          cache += ((state,collect(state)))
          collect(state)
        }else {
          val nextStates = getNextState(state)
          //println(nextStates)
          var maxChild = 0
          for (nextState <- nextStates) {
            val localMax = itr(nextState)
            if (localMax > maxChild) {
              maxChild = localMax
            }
          }

          cache += ((state,maxChild + collect(state)))
          maxChild + collect(state)
        }
      }else {
        cache.get(state).get
      }
    }

    val i1 = new Index(0,0)
    val i2 = new Index(0,grid(0).length-1)
    val state = new State(i1,i2)
    itr(state)
    //maxTotalCollected
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(Array(3,1,1),Array(2,5,1),Array(1,5,5),Array(2,1,1))
    println(cherryPickup(grid))
  }
}