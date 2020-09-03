import scala.collection.mutable

class Graph(val n : Int) {
  val conflictEdges = new mutable.HashMap[Int,mutable.HashSet[Int]]()
  for (j <- 1 to n) {
    conflictEdges += ((j,new mutable.HashSet[Int]()))
  }

  def addConflict(j : Int, k : Int): Unit = {
    conflictEdges.get(j).get.add(k)
    conflictEdges.get(k).get.add(j)
  }

  def getAllConflicts(j : Int) : Set[Int] = {
    conflictEdges.get(j).get.toSet
  }
}

case class Index(val index : Int,val s1 : Set[Int],val s2 : Set[Int])
object Solution {
  def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {
    val graph = new Graph(N)
    for (dislike <- dislikes) {
      val j = dislike(0)
      val k = dislike(1)

      graph.addConflict(j,k)
    }

    val cache = new mutable.HashMap[Index,Boolean]()
    def itr(dislikeIndex : Int,g1 : Set[Int],g2 : Set[Int]) : Boolean = {
      if (dislikeIndex == dislikes.length) {
        //all are consumed
        true
      }else {
        val index = new Index(dislikeIndex,g1,g2)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val dislike = dislikes(dislikeIndex)
          val j = dislike(0)
          val k = dislike(1)

          val retValueOptions = new mutable.HashSet[Boolean]()
          if (graph.getAllConflicts(j).intersect(g1.toSet).isEmpty && graph.getAllConflicts(k).intersect(g2.toSet).isEmpty) {
            retValueOptions.add(itr(dislikeIndex + 1, g1.+(j), g2.+(k)))
          }

          if (graph.getAllConflicts(k).intersect(g1.toSet).isEmpty && graph.getAllConflicts(j).intersect(g2.toSet).isEmpty) {
            retValueOptions.add(itr(dislikeIndex + 1, g1.+(k), g2.+(j)))
          }

          cache += ((index,retValueOptions.contains(true)))
          retValueOptions.contains(true)
        }
      }
    }

    itr(0,Set(),Set())
  }
}