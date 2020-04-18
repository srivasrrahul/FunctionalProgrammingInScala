import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class Order(val ch1 : Char,val ch2 : Char)
class DirectedGraph {
  val nodes = new mutable.HashMap[Char,mutable.HashSet[Char]]()

  def addKnowledge(order: Order) : Unit = {
    //println("Adding order " + order)
    nodes.get(order.ch1) match {
      case Some(set) => {
        set.add(order.ch2)
      }
      case None => {
        val set = new mutable.HashSet[Char]()
        set.add(order.ch2)

        nodes += ((order.ch1,set))
      }
    }

    //Add dest in graph if there is none
    nodes.get(order.ch2) match {
      case None => {
        //add node
        val set = new mutable.HashSet[Char]()

        nodes += ((order.ch2,set))
      }
      case _ => {

      }
    }
  }

  def addAllChars(charSet : mutable.HashSet[Char]) : Unit = {
    charSet.foreach(ch => {
      nodes.get(ch) match {
        case None => {
          val set = new mutable.HashSet[Char]()

          nodes += ((ch,set))
        }
        case _ => {}

      }
    })
  }

  def linearOrder() : String = {
    var time = 0
    def incTime() : Int = {
      time += 1
      time
    }

    val visited = new mutable.HashSet[Char]()
    //val postMap = new mutable.HashMap[Char,Int]()
    val postMap = new mutable.HashMap[Char,Int]
    val preMap = new mutable.HashMap[Char,Int]()

    var cycle = false
    def explore(current : Char) : Unit = {
      //println("   Exploring " + current)
      visited.add(current)
      preMap += ((current,incTime()))


      breakable {
        nodes.get(current) match {
          case Some(set) => {
            //println("For current " + current + " => " + set)
            set.foreach(dest => {
              if (visited.contains(dest) == false) {


                explore(dest)
              }else {
                if (preMap.contains(dest) && postMap.contains(dest) == false) {
                  //Cycle is true
                  //println("Cylce is there")
                  cycle = true
                  break
                }
              }
            })
          }
          case _ => {

          }
        }
      }

      postMap += ((current,incTime()))

    }

    nodes.foreachEntry((ch,_) => {
      if (visited.contains(ch) == false) {
        explore(ch)
      }
    })



    if (cycle == false) {
      val arr = new Array[(Char,Int)](nodes.size)
      var j = 0
      postMap.foreachEntry((postTime,ch) => {
        arr(j) = ((postTime,ch))
        j  = j + 1
      })

      //println(postMap)
      arr.sortInPlace()(new Ordering[(Char,Int)] {
        override def compare(x: (Char, Int), y: (Char, Int)): Int = {
          y._2.compareTo(x._2)
        }
      })


      val stringBuilder = new StringBuilder
      for (elem <- arr) {
        stringBuilder.append(elem._1)
      }

      stringBuilder.toString()
    }else {
      ""
    }

  }
}
object Solution {
  def order(s1 : String,s2 : String) : (Option[Order],(mutable.HashSet[Char],Boolean)) = {
    val minLength = if (s1.length > s2.length) s2.length else s1.length
    var retValue : Option[Order] = None
    val allChars = new mutable.HashSet[Char]()
    var isAnyDiff = false
    breakable {
      for (j <- 0 to minLength - 1) {
        if (s1(j) != s2(j)) {
          retValue = Some(new Order(s1(j),s2(j)))
          isAnyDiff = true
          break
        }
      }
    }

    if (isAnyDiff == false && s1.length > s2.length) {

        //Invalid
        (None,(new mutable.HashSet[Char](),false))
    }
    else {
      s1.foreach(ch => {
        allChars.add(ch)
      })

      s2.foreach(ch => {
        allChars.add(ch)
      })

      (retValue,(allChars,true))
    }
  }

  def alienOrder(words: Array[String]): String = {


    var graph = new DirectedGraph
    var first = words(0)

    val firstSet = new mutable.HashSet[Char]()
    first.foreach(ch => {
      firstSet.add(ch)
    })

    graph.addAllChars(firstSet)

    var isInvalid = false

    for (j <- 1 to words.length-1) {
      val s1 = first
      val s2 = words(j)
      order(s1,s2) match {
        case (Some(ord),(allChars,_)) => {
          graph.addKnowledge(ord)
          graph.addAllChars(allChars)
        }
        case (None,(allChars,true)) => {
          graph.addAllChars(allChars)

        }
        case (_,(_,false)) => {
          isInvalid = true
        }
      }

      first = words(j)
    }

    if (isInvalid == false) {
      graph.linearOrder()
    }
    else {
      ""
    }

  }

  def main(args: Array[String]): Unit = {

//    val words = Array("wrt",
//      "wrf",
//      "er",
//      "ett",
//      "rftt")


    //val words = Array("abc","ab")

    val words = Array("abcd")

    val lst = alienOrder(words)
    println(lst)
  }
}