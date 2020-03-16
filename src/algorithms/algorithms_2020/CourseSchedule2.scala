import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class PrePostValue(_indexId : Int = 0)
{
  var indexId = _indexId
  var preValue = Int.MaxValue
  var postValue = Int.MaxValue


}

class ValueOrdering extends Ordering[PrePostValue] {
  override def compare(x: PrePostValue, y: PrePostValue): Int = {
    x.postValue.compare(y.postValue)
  }
}


class Graph(val _numVertex : Int) {
  val vertices = new Array[ListBuffer[Int]](_numVertex)
  for (j <- 0 to _numVertex-1) {
    vertices(j) = new ListBuffer[Int]
  }

  def addEdge(source : Int,dest : Int) : Unit = {
    vertices(source).addOne(dest)
  }

  def linearOrder() : Array[Int] = {
    val visited = Array.ofDim[Boolean](_numVertex)
    for (j <- 0 to visited.length-1) {
      visited(j) = false
    }


    val prePostValues = Array.ofDim[PrePostValue](_numVertex)
    for (j <- 0 to _numVertex-1) {
      prePostValues(j) = new PrePostValue(j)
      //prePostValues(j).indexId = j
    }



    var cycleExists = false


    var clock = 1
    def explore(v : Int) : Unit = {
      //println("Exploring v " + v)
      visited(v) = true

      prePostValues(v).preValue = clock
      clock = clock + 1

      //println("Neighbours length for v " + v + " => " + vertices(v).length)
      vertices(v).foreach(neigbour => {
        if (visited(neigbour) == true) {
          if (prePostValues(neigbour).postValue == Int.MaxValue && prePostValues(neigbour).preValue < prePostValues(v).preValue ) {
            cycleExists = true
          }
        }else {
          explore(neigbour)
        }
      })


      prePostValues(v).postValue = clock
      clock = clock + 1
    }

    for (j <- 0 to _numVertex-1) {
      if (visited(j) == false) {
        explore(j)
      }
    }

    if (cycleExists == true) {
      new Array[Int](0)
    }else {
//      for (j <- 0 to prePostValues.length-1) {
//        println("For index " + j + " pre-value = " + prePostValues(j).preValue + " postValue = " + prePostValues(j).postValue)
//      }
      val l2 = prePostValues.sortWith(_.postValue > _.postValue)


      val retValue = new Array[Int](_numVertex)
      for (j <- 0 to _numVertex-1) {
        retValue(j) = l2(j).indexId
      }

      retValue
    }
  }


}
object Solution {
  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val graph = new Graph(numCourses)
    for (preReq <- prerequisites) {
      graph.addEdge(preReq(1),preReq(0))
    }

    graph.linearOrder()
  }

  def main(args: Array[String]): Unit = {
    val a1 = Array(0,1)
    //     val a2 = Array(1,0)
    var arr = Array(a1)
    //var arr = Array[Array[Int]](a)

    println(findOrder(2,arr).mkString(","))
  }
}