import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Solution {
  def findDiagonalOrder(nums: List[List[Int]]): Array[Int] = {
    val arrBuffer = new ArrayBuffer[Int]()
    val lstMap = new mutable.TreeMap[Int,(List[Int],Int)]()

    var j = 0
    var iterationOrder = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()
    for (num <- nums) {
      lstMap += ((j,(num,0)))

      val defaultSet = iterationOrder.getOrElseUpdate(0,new mutable.TreeSet[Int]())
      defaultSet.add(j)
      j = j + 1
    }

    println(iterationOrder)
    println(lstMap)

    println("============")
    //println(lstMap)
    var added = false


    val retValue = new ArrayBuffer[Int]()
    do {
      added = false
      //val newIterationOrder = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()

      for ((_,ordered) <- iterationOrder) {
        //println(iterationOrder)

        for (lstIndex <- ordered) {
          val lstBeforeThis = lstMap.rangeTo(lstIndex)
          val localAccumulator = new ListBuffer[Int]
          for ((prevLstIndex,(lst,lstCount)) <- lstBeforeThis) {
            if (lst.isEmpty == false) {
              localAccumulator.append(lst.head)
              lstMap += ((prevLstIndex, ((lst.tail, lstCount + 1))))
              added = true
            }
          }

          //println(localAccumulator.toList)
          retValue.appendAll(localAccumulator.reverse)

        }


      }


//      println("==========")
//      println(retValue)
//      //println(newIterationOrder)
//      println(lstMap)
      val newOrder = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()
      for ((lstIndex,(lst,lstTop)) <- lstMap) {
        if (lst.isEmpty == false) {
          val defaultSet = newOrder.getOrElseUpdate(lstTop,new mutable.TreeSet[Int]())
          defaultSet.add(lstIndex)
        }
      }
      iterationOrder = newOrder


    } while (added == true)



    //println("Final arr" + arrBuffer.toList)
    retValue.toArray

  }

  def main(args: Array[String]): Unit = {
    println(findDiagonalOrder(List(List(1,2,3,4,5),List(6,7),List(8),List(9,10,11),List(12,13,14,15,16))).mkString(","))
  }
}