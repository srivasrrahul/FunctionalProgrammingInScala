import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Graph {
  val nodesEqual = new mutable.HashMap[Char,mutable.HashSet[Char]]()
  val nodesNotEqual = new mutable.HashMap[Char,mutable.HashSet[Char]]()

  def addEqual(u : Char,v : Char) : Unit = {
    if (u != v) {
      //No self nodes
      val equalNodes = nodesEqual.getOrElseUpdate(u,new mutable.HashSet[Char]())
      equalNodes.add(v)
    }

  }

  def addNotEqual(u : Char,v : Char) : Unit = {
    val neNodes = nodesNotEqual.getOrElseUpdate(u,new mutable.HashSet[Char]())
    neNodes.add(v)
  }

  var colorId = 0
  def newColor() : Int = {
    colorId = colorId + 1
    colorId
  }
  val colorsAssigned = new mutable.HashMap[Char,Int]()

  def bfs(s : Char) : Set[Char] = {
    val q = new mutable.Queue[Char]()
    q.addOne(s)

    val visited = new mutable.HashSet[Char]()

    while (q.isEmpty == false) {
      val top = q.dequeue()
      visited.add(top)

      for (v <- nodesEqual.getOrElse(top,new mutable.HashSet[Char]())) {
        if (visited.contains(v) == false) {
          q.addOne(v)
        }
      }


    }

    visited.toSet
  }


  def checkColor(u : Char) : Boolean = {
    val neNodes = nodesNotEqual.getOrElse(u,new mutable.HashSet[Char]())

    val cantAssign = new mutable.HashSet[Int]()
    for (neNode <- neNodes) {
      cantAssign.add(colorsAssigned.getOrElse(neNode,0))
    }

    //remove 0
    cantAssign.remove(0)
    val currentColor = colorsAssigned.get(u).get
    if (cantAssign.contains(currentColor)) {
      false
    }else {
      true
    }

  }

  def assignEqual(s : Char) : Unit = {
    val tree = bfs(s)
    val col = newColor()
    for (v <- tree) {
      colorsAssigned += ((v,col))
    }

    colorsAssigned += ((s,col))
  }



  //a==b,b==c,c == b,d == c,c != a
  //

}

object Solution {
  def equationsPossible(equations: Array[String]): Boolean = {
    var valid = true
    val graph = new Graph
    val totalNodes = new mutable.HashSet[Char]()
    for (eq <- equations) {
      if (eq.contains("==")) {
        if (eq.head != eq.last) {
          totalNodes.add(eq.head)
          totalNodes.add(eq.last)
          graph.addEqual(eq.head,eq.last)
          graph.addEqual(eq.last,eq.head)
        }

      }else {
        if (eq.head != eq.last) {
          totalNodes.add(eq.head)
          totalNodes.add(eq.last)
          graph.addNotEqual(eq.head,eq.last)
          graph.addNotEqual(eq.last,eq.head)
        }else {
          valid = false
        }
      }
    }

    if (valid == true) {

      //Assign equal colors
      while (graph.colorsAssigned.size < totalNodes.size) {
        for (u <- totalNodes if graph.colorsAssigned.contains(u) == false) {
          graph.assignEqual(u)
        }
      }

      for (u <- totalNodes if valid == true) {
        valid = graph.checkColor(u) //asset again per ite
      }

    }

    //println(valid + " " + graph.colorsAssigned + " " + graph.nodesEqual + " ")

    valid


  }
}