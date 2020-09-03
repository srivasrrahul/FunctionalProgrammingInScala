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

    val visited = new mutable.HashSet[Int]()
    val colors = new mutable.HashMap[Int,Int]()
    val parents = new mutable.HashMap[Int,Int]()

    def getParentColor(j : Int) : Int = {
      //0 means no color
      if (parents.contains(j)) {
        colors.get(parents.get(j).get).get
      }else {
        0
      }
    }
    val q = new mutable.Queue[Int]()

    var possible = true
    def bfs() : Unit = {
      while (q.isEmpty == false && possible == true) {
        val top = q.dequeue()
        val parentCol = getParentColor(top)

        if (parentCol == 0) {
          colors += ((top, 1))
        } else {
          val newColor = if (parentCol == 1) 2 else 1

          colors += ((top, if (parentCol == 1) 2 else 1))
        }

        visited.add(top)

        for (j <- graph.getAllConflicts(top)) {
          if (visited.contains(j) == false) {
            parents += ((j, top))
            q.append(j)
          } else {
            //Check its colore
            val jColor = colors.get(j).get
            if (jColor == colors.get(top).get) {
              possible = false
            }
          }
        }

      }
    }

    if (possible == true) {
      for (j <- 1 to N) {
        if (visited.contains(j) == false) {
          q.addOne(j)
          bfs()
        }
      }
    }
    possible

  }
}