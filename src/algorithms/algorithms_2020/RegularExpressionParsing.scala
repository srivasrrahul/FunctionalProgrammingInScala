import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DirectedNode(val id : Int,var _value : Char) {
  var nextEdges  = new ListBuffer[DirectedNode]()
  var value : Char = _value
  val epslionTransition = new mutable.ListBuffer[DirectedNode]()

  def addEdge(directedNode: DirectedNode) : Unit = {
    nextEdges.append(directedNode)
  }

  def addEpsilonTransiton(directedNode: DirectedNode) : Unit = {
    epslionTransition.append(directedNode)
  }

  def getNextEdge(eventChar : Char) : List[DirectedNode] = {
    if (eventChar == value) {
      nextEdges.toList
    }else {
      if (value == '.') {
        nextEdges.toList
      }else {
        List()
      }

    }
  }

  def getNextEpsilon() : List[DirectedNode] = {
    epslionTransition.toList
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedNode]

  override def equals(other: Any): Boolean = other match {
    case that: DirectedNode =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }


  override def toString = s"DirectedNode($id,$value,$nextEdges)"
}

class DirectedGraph(val count : Int,val idLst : String) {
  val directedNodes = new Array[DirectedNode](count)
  for (j <- 0 to count-1) {
    directedNodes(j) = new DirectedNode(j,idLst(j))
  }

  for (j <- 0 to count-1) {
    val ch = idLst(j)
    ch match {
      case letter if ch.isLetter || ch == '.' => {
        if (j+1 < count) {
          directedNodes(j).addEdge(directedNodes(j + 1))
        }
      }
      case '(' => {
        if (j+1 < count) {
          directedNodes(j).addEpsilonTransiton(directedNodes(j+1))
        }
      }
      case '*' => {
        if (j-1 >= 0) {
          directedNodes(j-1).addEpsilonTransiton(directedNodes(j))
          directedNodes(j).addEpsilonTransiton(directedNodes(j-1))
        }

        if (j+1 < count) {
          directedNodes(j).addEpsilonTransiton(directedNodes(j+1))
        }
      }
      case _ => {

      }
    }

  }

  def depthFirstSearch(sourceNode : DirectedNode) : List[DirectedNode] = {
    val visited = new mutable.HashSet[Int]()
    def explore(source : DirectedNode) : Unit = {
      visited.add(source.id)

      for (epsilonNode <- source.getNextEpsilon()) {
        if (visited.contains(epsilonNode.id) == false) {
          explore(epsilonNode)
        }
      }
    }

    explore(sourceNode)
    val lst = new ListBuffer[DirectedNode]
    for (nodeId <- visited) {
      lst.append(directedNodes(nodeId))
    }

    lst.toList
  }


  def debug() : Unit = {
    for (directedNode <- directedNodes) {
      println(directedNode.toString)
    }
  }



}

object Solution {
  def isMatch(s: String, p: String): Boolean = {
    val graph = new DirectedGraph(p.length+2,"(" + p + ")")
    //graph.debug()
    val startState = graph.directedNodes(0)
    var currentStates = new mutable.HashSet[DirectedNode]()
    val initialEpsion = graph.depthFirstSearch(startState)

//    println("Begin state " + startState + " Epsilon " + startState.getNextEpsilon())
    currentStates.addAll(initialEpsion)
    currentStates.add(startState)

    println("Begin")
    println(currentStates.mkString("\n"))
    println("End")

    for (ch <- s) {
      //println("===========================")
      val nextStates = new mutable.HashSet[DirectedNode]()

      for (currentState <- currentStates) {
        //println(ch + " " + currentState.value + " " + currentState.getNextEdge(ch))
        nextStates.addAll(currentState.getNextEdge(ch))
      }

      //println("For  " + ch + " next states " + nextStates)
      val nextEpsilonStates = new mutable.HashSet[DirectedNode]()
      for (nextState <- nextStates) {
        //println("For  " + nextState + " next epsilon " + nextState.epslionTransition)
        nextEpsilonStates.addAll(graph.depthFirstSearch(nextState))
      }



      //println("For  " + ch + " next epsilor states " + nextEpsilonStates)

      nextEpsilonStates.addAll(nextStates)

      //println("For  " + ch + " next cumulative state " + nextEpsilonStates)

      currentStates = nextEpsilonStates
    }

    //println(currentStates)
    val lastNode = graph.directedNodes.last
    if (currentStates.contains(lastNode)) {
      true
    }else {
      false
    }
    //false

  }

  def main(args: Array[String]): Unit = {
    println(isMatch("aaa","ab*ac*a"))
  }
}