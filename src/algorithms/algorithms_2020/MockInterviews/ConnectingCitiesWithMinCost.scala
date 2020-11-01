import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
case class Maps(N : Int) {
  val s = new mutable.HashMap[Int, (Int,mutable.HashSet[Int])]()
  for (j <- 1 to N) {
    //val m = new mutable.HashMap[Int,mutable.HashSet[Int]]()
    s += ((j,(j,mutable.HashSet[Int](j))))
  }

  def find(u : Int) : Int = {
    s.get(u).get._1
  }

  def union(u : Int,v : Int) : Unit = {
    val uId = find(u)
    val vId = find(v)

    if (uId < vId) {
      val uSet = s.get(uId).get._2
      val vSet = s.get(vId).get._2

      uSet.addAll(vSet)
      for (vElem <- vSet) {
        s += ((vElem,(uId,uSet)))
      }
    }else {
      union(v,u)
    }
  }

  def checkAll() : Boolean = {
    var first = s.head._2._1
    var connected = true
    for ((_,(sId,_)) <- s if connected == true) {
      if (sId != first) {
        connected = false
      }
    }

    connected
  }


}

case class Edge(val u : Int,val v : Int,val c : Int)
object Solution {
  def minimumCost(N: Int, connections: Array[Array[Int]]): Int = {
    //has smallest element as the maps key
    val edgges = new ArrayBuffer[Edge]()
    for (connection <- connections) {
      edgges.addOne(new Edge(connection(0),connection(1),connection(2)))
    }

    edgges.sortInPlace()(new Ordering[Edge] {
      override def compare(x: Edge, y: Edge): Int = {
        x.c.compare(y.c)
      }
    })

    val maps = new Maps(N)
    var totalCost = 0
    for (edge <- edgges) {
      val u = edge.u
      val v = edge.v

      if (maps.find(u) != maps.find(v)) {
        totalCost = totalCost + edge.c
        maps.union(u,v)
      }
    }

    if (maps.checkAll()) {
      totalCost
    }else {
      -1
    }


  }
}