import scala.collection.mutable

case class Vector(val position : Int,val speed : Int)
trait Msg
case object MsgA extends Msg
case object MsgR extends Msg

object MsgHandler {
  def handle(vector: Vector,msg : Msg) : Vector = {
    msg match {
      case MsgA => {
        val newPos = vector.position + vector.speed
        val newSpeed = vector.speed*2
        new Vector(newPos,newSpeed)
      }
      case MsgR => {
        var newSpeed = 0
        if (vector.speed > 0) {
          newSpeed = -1
        }else {
          newSpeed = 1
        }

        new Vector(vector.position,newSpeed)
      }
    }
  }
}
object Solution {
  def racecar(target: Int): Int = {
    val initialVector = new Vector(0, 1)
    //var minDistance = Int.MaxValue

    val pq = mutable.PriorityQueue.empty[(Vector, Int)](new Ordering[(Vector, Int)] {
      override def compare(x: (Vector, Int), y: (Vector, Int)): Int = {
        val d1 = math.abs(x._1.position)
        val d2 = math.abs(y._1.position)
        d2.compareTo(d1)
      }
    })

    val visited = new mutable.HashSet[Vector]()

    pq.addOne((initialVector, 0))
    var goalFound = false

    var foundTargetDistance = Int.MaxValue
    while (pq.isEmpty == false && goalFound == false) {
      val top = pq.dequeue()
      visited.add(top._1)
      if (top._1.position == target) {
        foundTargetDistance = top._2
        goalFound = true
      } else {
        val nextMsgA = MsgHandler.handle(top._1, MsgA)
        val nextMsgR = MsgHandler.handle(top._1, MsgR)

        if (visited.contains(nextMsgA) == false) {
          pq.addOne((nextMsgA, top._2 + 1))
        }

        if (visited.contains(nextMsgR) == false) {
          pq.addOne((nextMsgR, top._2 + 1))
        }
      }
    }

    //Goal found with some path
    //Check whatever was pending in pq if that traverses to distance and found that

    val pq1 = mutable.PriorityQueue.empty[(Vector, Int)](new Ordering[(Vector, Int)] {
      override def compare(x: (Vector, Int), y: (Vector, Int)): Int = {
        //get shortest distance
        y._2.compareTo(x._2)
      }
    })


    visited.clear()
    pq.clear()
    //Apply dijkstra

    pq1.addOne((initialVector,0))
    val upperBound = foundTargetDistance
    foundTargetDistance = 0
    val distanceMap = new mutable.HashMap[Vector,Int]()
    //println(upperBound)
    var found = false
    while (pq1.isEmpty == false && found == false) {
      val top = pq1.dequeue()
      val newDistance = top._2+1
      if (top._1.position == target) {
        //println("Hee " + top._2)
        //distanceMap += ((top._1,newDistance))
        found = true
      }

      //print(pq1.size + ",")


      if (newDistance >= upperBound) {
        //forget it
      } else {
        val nextMsgA = MsgHandler.handle(top._1, MsgA)
        val nextMsgR = MsgHandler.handle(top._1, MsgR)
        val existingDistanceA = distanceMap.getOrElse(nextMsgA,Int.MaxValue)
        if (newDistance < existingDistanceA) {
          distanceMap += ((nextMsgA,newDistance))
          pq1.addOne((nextMsgA, newDistance))
        }

        val existingDistanceR = distanceMap.getOrElse(nextMsgR,Int.MaxValue)
        if (newDistance < existingDistanceR) {
          distanceMap += ((nextMsgR,newDistance))
          pq1.addOne((nextMsgR, newDistance))
        }

      }
    }

    var minDistance = Int.MaxValue
    for ((vec,distance) <- distanceMap) {
      if (vec.position == target) {
        if (distance < minDistance) {
          minDistance = distance
        }
      }
    }

    math.min(minDistance,upperBound)

  }
}