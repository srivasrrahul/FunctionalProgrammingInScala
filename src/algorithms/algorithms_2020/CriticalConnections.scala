import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._


case class Edge(val source : Int,val dest : Int)
class Graph(val _n : Int) {
  val nodeCount = _n
  val nodeEdges = new mutable.HashMap[Int,List[Edge]]()

  for (j <- 0 to nodeCount-1) {
    nodeEdges += ((j,List[Edge]()))
  }

  def addEdge(edg: List[Int]) : Unit = {


    nodeEdges.get(edg.head) match {
      case Some(edgeLst : List[Edge]) => {
        val edge = new Edge(edg.head,edg.tail.head)
        nodeEdges += ((edg.head,edge::edgeLst))
      }
      case None => {

      }
    }

    nodeEdges.get(edg.tail.head) match {
      case Some(edgeLst : List[Edge]) => {
        val edge = new Edge(edg.tail.head,edg.head)
        nodeEdges += ((edg.tail.head,edge::edgeLst))
      }
      case None => {

      }
    }
  }

  def getCriticalEdges() : List[List[Int]] = {

    val preTime = new mutable.HashMap[Int,Int]()
    val postTime = new mutable.HashMap[Int,Int]()

    var time = 0
    val visited = new mutable.HashSet[Int]()

    def incTime() : Int = {
      time = time+1
      time
    }

    val path = new mutable.HashMap[Int,(Edge,Boolean)]() //node was visited by that edge


    def explore(currentVertex : Int) : Unit = {
      visited.add(currentVertex)
      preTime += ((currentVertex,incTime()))

      //println("Exploring " + currentVertex)
      nodeEdges.getOrElse(currentVertex,List[Edge]()).foreach(edge => {
        //println("Next edge : " + edge)
        val currentDest = edge.dest

        if (visited.contains(currentDest) == false) {
          //println("Not visited hence adding" + currentDest)
          path += ((currentDest,(edge,true)))
          explore(currentDest)
        }else {
          if (preTime.getOrElse(currentDest,0) > 0 && postTime.getOrElse(currentDest,0) == 0 && path.get(currentVertex).get._1.source != currentDest) {
            //In dest cycle
            //println("In dest cycle " + currentDest)
            breakable {
              var currentNode = currentVertex
              while (true) {
                path.get(currentNode) match {
                  case Some((edge,isCriticalPath)) => {
                    //println("Extracing old edge for marking it non critical " + edge)
                    path += ((currentNode,(edge,false)))
                    currentNode = edge.source
                    if (currentNode == currentDest) {
                      break()
                    }
                  }
                  case None => {
                    break
                  }
                }
              }
            }
          }
        }



      })

      postTime += ((currentVertex,incTime()))

    }

    nodeEdges.foreachEntry((vertex,_) => {
      if (visited.contains(vertex) == false) {
        explore(vertex)
      }
    })


    val retValue = new ListBuffer[List[Int]]
    for (p <- path) {
      p match {
        case (_,(edge,true)) => {
          retValue.addOne(List(edge.source,edge.dest))
        }
        case _ => {

        }
      }
    }


//    val retValue = path.foldRight (new ListBuffer[List[Int]]) {
//      (entry,z) => {
//        entry match {
//          case (_,(edge,true)) => {
//            z.addOne(List(edge.source,edge.dest))
//          }
//          case _ => {
//
//          }
//        }
//
//        z
//      }
//    }

    retValue.toList
  }

  def print() : Unit = {
    nodeEdges.foreachEntry((vertex,lst : List[Edge]) => {
      println("Vertex " + vertex + " edge " + lst.mkString(","))
    })
  }
}
object Solution {
  def criticalConnections(n: Int, connections: List[List[Int]]): List[List[Int]] = {
    val graph = new Graph(n)
    connections.foreach(connection => {
      graph.addEdge(connection)
    })


    //graph.print()
    graph.getCriticalEdges()
  }

  def testCase1(): Unit = {
    println(criticalConnections(4,List()))
  }

  def testCase2(): Unit = {
    println(criticalConnections(4,List(List(0,1),List(1,2),List(2,0),List(1,3))))
  }

  def testCase3(): Unit = {
    println(criticalConnections(6,List(List(0,1),List(1,2),List(2,0),List(1,3),List(3,4),List(4,5),List(5,3))))
  }

  def testCase4(): Unit = {
    println(criticalConnections(6,List(List(0,1),List(1,2),List(2,0),List(3,4),List(4,5),List(5,3))))
  }

  def executeLargeInput(filePath : String) : Unit = {

  }
  def main(args: Array[String]): Unit = {
    testCase3()

  }
}