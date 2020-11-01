import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
case class Maps(N : Int) {
  val s = new mutable.HashMap[Int, (Int,mutable.HashSet[Int])]()
  for (j <- 0 to N-1) {
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


object Solution {
  def validateBinaryTreeNodes(n: Int, leftChild: Array[Int], rightChild: Array[Int]): Boolean = {
    val maps = new Maps(n)
    var connectedTree = true
    for (j <- 0 to n-1) {
      val u = leftChild(j)
      val v = rightChild(j)

      if ((u != -1 && maps.find(u) == maps.find(j)) || (v != -1 && maps.find(v) == maps.find(j))||
        (u != -1 && v != -1 && maps.find(u) == maps.find(v))) {
        connectedTree = false
      }else {
        if (u != -1) {
          maps.union(j,u)
        }

        if (v != -1) {
          maps.union(j,v)
        }


      }
    }

    if (connectedTree && maps.checkAll()) {
      true
    }else {
      false
    }
  }


}