import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def killProcess(pids: List[Int], ppids: List[Int], kill: Int): List[Int] = {
    var j = 0
    val processPIdMap  = new mutable.HashMap[Int,Int]() //process id and pid-array index
    val processPPIdMap  = new mutable.HashMap[Int,Set[Int]]() //pp id and child processid map
    for (pid <- pids) {
      processPIdMap += ((j,pid))

      j = j + 1
    }

    j = 0
    for (ppid <- ppids) {
      val existingLst = processPPIdMap.getOrElseUpdate(ppid,Set[Int]())
      processPPIdMap += ((ppid, existingLst.+(processPIdMap.get(j).get)))
      j = j + 1
    }


    val killedPId = new ListBuffer[Int]
    val queue = new mutable.Queue[Int]
    queue.addOne(kill)

    while (queue.isEmpty == false) {
      val allPids = queue.dequeueAll(_ => true)
      for (pid <- allPids) {
        killedPId.append(pid)
        queue.appendAll(processPPIdMap.get(pid).getOrElse(Set()))
        //Find their child
      }
    }

    killedPId.toList

  }

  def main(args: Array[String]): Unit = {
    println(killProcess(List(1,3,10,5),List(3,0,5,3),3))
  }
}