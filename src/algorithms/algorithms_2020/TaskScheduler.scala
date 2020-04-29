import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks._

object Solution {
  def leastInterval(tasks: Array[Char], n: Int): Int = {
    val taskCount = new mutable.HashMap[Char,Int]()
    val countTask = new mutable.TreeMap[Int,mutable.HashSet[Char]]()(Ordering[Int].reverse)

    for (task <- tasks) {
      val oldCount = taskCount.getOrElseUpdate(task,0)
      taskCount += ((task,oldCount+1))
    }

    taskCount.foreachEntry((task,count) => {
      countTask.getOrElseUpdate(count,new mutable.HashSet[Char]).add(task)
    })

    val cantPickTask = new mutable.HashMap[Char,Int]()

    def pickTask() : Option[Char] = {
      cantPickTask.mapValuesInPlace((_,count) => count+1) //inceease all by one
      cantPickTask.filterInPlace((_,count) => count <= n) //remove task which are expired
      //finish highest one first be greedy which doesnt lie in cantpick task
      var taskCountVal : Option[(Char,Int)] = None
      breakable {
        for (countLst <- countTask) {
          for (task <- countLst._2) {
            if (cantPickTask.contains(task) == false) {
              taskCountVal = Some((task,countLst._1))
              break()
            }
          }
        }
      }

      taskCountVal match {
        case None => {
          None
        }
        case Some((pickedTask,currentCount)) => {
          countTask.get(currentCount).get.remove(pickedTask)
          if (countTask.get(currentCount).get.size == 0) {
            countTask.remove(currentCount)
          }

          val newCount = currentCount-1
          if (newCount > 0) {
            countTask.getOrElseUpdate(newCount, new mutable.HashSet[Char])

            countTask.get(newCount).get.add(pickedTask)
          }
          Some(pickedTask)
        }
      }
    }

    var cycles = 0

    while (countTask.isEmpty == false) {
      cycles = cycles + 1 //in curent cycle
      val pickedTask = pickTask()
      //println("Picked " + pickedTask)
      pickedTask match {
        case None => {
          //println("Picked element ")
        }
        case Some(taskName) => {
          cantPickTask += ((taskName,0))
        }
      }

      //println(countTask)
      //println(cantPickTask)
    }

    cycles

  }

  def main(args: Array[String]): Unit = {
    println(leastInterval(Array('A','A','A','B','B','B'),2))
  }
}