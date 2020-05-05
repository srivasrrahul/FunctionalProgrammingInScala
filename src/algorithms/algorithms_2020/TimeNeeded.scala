import scala.collection.mutable.ListBuffer

object Solution {
  def numOfMinutes(n: Int, headID: Int, manager: Array[Int], informTime: Array[Int]): Int = {
    //val timeIndex = new Array[Int](manager.length)
    val timeIndex = Array.fill[Int](manager.length)(-1)
    for (j <- 0 to manager.length-1) {
      if (manager(j) != -1) {
        if (timeIndex(j) == -1) {
          var k = j
          val pathToManager = new ListBuffer[Int]
          while (k != -1) {
            pathToManager.append(k)
            k =  manager(k)
          }

          //println("For employee " + j + " manager path " + pathToManager.reverse.toList)
          val pathFromManager = pathToManager.reverse.toList
          val totalTime = pathFromManager.foldLeft(0)((totalTime,employee) => {
            //println("IN fold for " + employee)
            val totalTimeForEmployee = totalTime + informTime(employee)
            timeIndex(employee) = totalTimeForEmployee
            //println("Setting time for employee " + employee + " Time " + totalTimeForEmployee)
            totalTimeForEmployee
          })
        }
      }else {
        timeIndex(j) = 0 //time tellingg self is 0
      }
    }

    timeIndex.max

  }

  def main(args: Array[String]): Unit = {
    println(numOfMinutes(6,2,Array(2,2,-1,2,2,2),Array(0,0,1,0,0,0)))
  }
}