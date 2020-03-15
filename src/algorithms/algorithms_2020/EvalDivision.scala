import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._


class DirectedGraph {
  val graph = new mutable.HashMap[String,mutable.HashMap[String,Double]]()

  def add(lst : List[String],value : Double): Unit = {
    //list is of two elements
    val first = lst.head
    val second = lst.tail.head

    val rev = 1.0/value

    graph.get(first) match {
      case Some(firstNext) => {
        firstNext += ((second,value))
      }
      case None => {
        val firstNext = new mutable.HashMap[String,Double]()
        firstNext += ((second,value))
        graph += ((first,firstNext))
      }
    }

    graph.get(second) match {
      case Some(secondNext : mutable.HashMap[String,Double]) => {
        secondNext += ((first,rev))
      }
      case None => {
        val secondNext = new mutable.HashMap[String,Double]()
        secondNext += ((first,rev))
        graph += ((second,secondNext))
      }
    }
  }

  def query(source : String,dest : String) : Double = {

    //do a bfs for directed graph
    val result = new mutable.HashMap[String,Double]()
    result += ((source,1.0))
    val visited = new mutable.HashSet[String]()

    val queue = new mutable.Queue[String]()
    queue.addOne(source)


    var noResult = true
    var finalValue = -1.0
    breakable {
      while (queue.isEmpty == false) {
        val u = queue.removeHead()
        graph.get(u) match {
          case Some(next : mutable.HashMap[String,Double]) => {
            next.foreach(neigbour => {
              val v = neigbour._1
              if (visited.contains(v) == false) {
                val vResult = result.getOrElse(u,1.0) * neigbour._2
                visited.add(v)
                queue.addOne(v)
                result += ((v,vResult))
                if (v == dest) {
                  noResult = false
                  finalValue = vResult
                  break()
                }
              }
            })
          }
          case None => {
            noResult = true
            break
          }
        }
      }
    }

    finalValue
  }
}

object Solution {
  def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {
    val graph = new DirectedGraph
    (equations zip values).map {
      case (eq,value) => {
        graph.add(eq,value)
      }
    }

    val retValue = Array.ofDim[Double](queries.length)
    var index = 0
    queries.map {
      case query => {
        retValue(index) = graph.query(query.head,query.tail.head)
        index += 1
      }
    }

    retValue
  }

  def main(args: Array[String]): Unit = {
    val eqs = List(List("a","b"),List("b","c"))
    val values = Array(2.0,3.0)
    val queries = List(List("a","a"))
    println(calcEquation(eqs,values,queries).mkString(","))
  }
}