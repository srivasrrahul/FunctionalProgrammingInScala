import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph(val _numVertex : Int) {
  val vertices = new Array[ListBuffer[Int]](_numVertex)
  for (j <- 0 to _numVertex-1) {
    vertices(j) = new ListBuffer[Int]
  }

  def addEdge(source : Int,dest : Int) : Unit = {
     vertices(source).addOne(dest)
  }

  def cycleAndConnectivityExists() : Boolean = {
    val visited = Array.ofDim[Boolean](_numVertex)
    for (j <- 0 to visited.length-1) {
      visited(j) = false
    }

    val preValues = Array.ofDim[Int](_numVertex)
    val postValues = Array.ofDim[Int](_numVertex)

    for (j <- 0 to _numVertex-1) {
      preValues(j) = Int.MaxValue
      postValues(j) = Int.MaxValue
    }

    var cycleExists = false


    var clock = 1
    def explore(v : Int) : Unit = {
      visited(v) = true

      preValues(v) = clock
      clock = clock + 1

      vertices(v).foreach(neigbour => {
        if (visited(neigbour) == true) {
          //cycle
//          println("Cycle exists" + " source = " + v + " neigbour = " + neigbour)
//          cycleExists = true
          if (postValues(neigbour) == Int.MaxValue) {
            cycleExists = true
          }
        }else {
          explore(neigbour)
        }
      })


      postValues(v) = clock
      clock = clock + 1
    }

    for (j <- 0 to _numVertex-1) {
      if (visited(j) == false) {
        explore(j)
      }
    }

    if (cycleExists == true) {
      false
    }else {
      true
    }
  }


}
object Solution {
  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val graph = new Graph(numCourses)
    for (preReq <- prerequisites) {
      graph.addEdge(preReq(1),preReq(0))
    }

    graph.cycleAndConnectivityExists()
  }

  def main(args: Array[String]): Unit = {
      val a1 = Array(0,1)
//     val a2 = Array(1,0)
    var arr = Array(a1)
    //var arr = Array[Array[Int]](a)

    println(canFinish(2,arr))
  }
}