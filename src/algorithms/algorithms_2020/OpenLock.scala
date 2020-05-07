import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Solution {
  def convertIntToString(nodeIntId : Int) : String = {
    val str = nodeIntId.toString
    val prefixLen = 4- str.length
    "0"*prefixLen + str
  }

  def openLock(deadends: Array[String], target: String): Int = {


    val deadendsSet = deadends.toSet


    def nextStates(currentInt : Int) : List[Int] = {
      val str = convertIntToString(currentInt)
      val retValue = new ListBuffer[Int]
      for (j <- 0 to str.length-1) {
        val changedStr = ((str.charAt(j).asDigit + 1) % 10).toString
        val newStr = str.substring(0,j) +  changedStr + str.substring(j+1)
        if (deadendsSet.contains(newStr) == false) {
          retValue.append(Integer.parseInt(newStr))
        }

        var backInt =str.charAt(j).asDigit - 1
        if (backInt < 0) {
          backInt = 9
        }

        val prevStr = backInt.toString
        val newBackStr = str.substring(0,j) +  prevStr + str.substring(j+1)
        if (deadendsSet.contains(newBackStr) == false) {
          retValue.append(Integer.parseInt(newBackStr))
        }

      }

      //println("For current  " + currentInt + " =>  " + retValue.toList)
      retValue.toList
    }

//    val pq = new mutable.PriorityQueue[(Int,Int)]()(new Ordering[(Int,Int)] {
//      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
//        x._1.compareTo(y._1)
//      }
//    })

    val pq = new mutable.Queue[(Int,Int)]()

    val visited = new mutable.HashSet[Int]()
    val distance = new mutable.HashMap[Int,Int]()
    distance += ((0,0))

    pq.addOne((0,0))

    if (deadendsSet.contains("0000")) {
      -1
    }else {
      val targetNodeInt = Integer.parseInt(target)
      breakable {
        while (pq.isEmpty == false) {
          val topNode = pq.dequeue()
          if (visited.contains(topNode._2) == false) {
            visited.add(topNode._2)
          }


          val nextNodes = nextStates(topNode._2)
          //println("For current " + topNode._2 + " next nodes " + nextNodes)
          for (nextNode <- nextNodes) {
            val currentDistance = distance.get(topNode._2).get + 1
            distance.get(nextNode) match {
              case None => {

                distance += ((nextNode, currentDistance))
                pq.addOne((currentDistance, nextNode))
              }
              case Some(oldDistance) => {
                if (oldDistance > currentDistance) {
                  distance += ((nextNode, currentDistance))
                  pq.addOne((currentDistance, nextNode))
                }
              }
            }

            if (nextNode == targetNodeInt) {
              break
            }
          }
        }
      }

      distance.get(Integer.parseInt(target)) match {
        case None => {
          -1
        }
        case Some(distValue) => {
          distValue
        }
      }
    }


  }

  def main(args: Array[String]): Unit = {
    //println(convertIntToString(9999))
    //println(openLock(Array("0201","0101","0102","1212","2002"),"0202"))
    println(openLock(Array("0201","0101","0102","1212","2002"),"0202"))
  }
}