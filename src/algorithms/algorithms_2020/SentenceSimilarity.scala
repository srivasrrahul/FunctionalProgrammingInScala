import scala.collection.mutable

class SimilarGraph {
  val nodeEdges = new mutable.HashMap[String,mutable.HashSet[String]]()

  def addSimilarity(source : String,dest : String) : Unit = {
    val defaultSourceSet = nodeEdges.getOrElseUpdate(source,new mutable.HashSet[String]())
    defaultSourceSet.add(dest)

    val defaultDestSet = nodeEdges.getOrElseUpdate(dest,new mutable.HashSet[String]())
    defaultDestSet.add(source)
  }

  def getNeigbours(source : String) : Set[String] = {
    nodeEdges.get(source) match {
      case None => Set()
      case Some(neigbours) => neigbours.toSet
    }
  }

  def ifSame(source : String,dest : String) : Boolean = {
    val q = new mutable.Queue[String]()
    val visited = new mutable.HashSet[String]()

    q.addOne(source)

    var found = false
    while (q.isEmpty == false && found == false) {
      val top = q.dequeue()
      visited.add(top)

      if (top == dest) {
        found = true
      }else {
        for (neigbour <- getNeigbours(top)) {
          if (visited.contains(neigbour) == false) {
            q.addOne(neigbour)
          }
        }
      }

    }

    found
  }
}
object Solution {
  def areSentencesSimilarTwo(words1: Array[String], words2: Array[String], pairs: List[List[String]]): Boolean = {
    if (words1.size != words2.size) {
      false
    }else {
      val graph = new SimilarGraph
      for (pair <- pairs) {
        graph.addSimilarity(pair.head, pair.tail.head)
      }

      var similar = true
      for ((w1, w2) <- words1 zip words2 if similar == true) {
        if (graph.ifSame(w1, w2) == false) {
          similar = false
        }
      }

      similar
    }
  }
}