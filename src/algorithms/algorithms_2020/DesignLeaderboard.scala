import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Leaderboard() {

  val scoreOrdering = mutable.PriorityQueue.empty[(Int,Int)](new Ordering[(Int,Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = {
      x._1.compareTo(y._1)
    }
  })
  val scores = new mutable.HashMap[Int,Int]()

  def addScore(playerId: Int, score: Int) : Unit = {
    if (scores.contains(playerId) == false) {
      scores += ((playerId,score))
      scoreOrdering.addOne((score,playerId))
    }else {
      val oldScore = scores.getOrElseUpdate(playerId, 0)
      scores += ((playerId, oldScore + score))
      val lst = new ListBuffer[(Int,Int)]

      var processed = false
      while (scoreOrdering.isEmpty == false && processed == false) {
        val top = scoreOrdering.dequeue()
        if (top._2 == playerId) {
          val newTop = (top._1+score,top._2)
          scoreOrdering.addOne(newTop)
          processed = true
        }else {
          lst.append(top)
        }
      }

      scoreOrdering.addAll(lst.toList)
    }
  }

  def top(K: Int): Int = {
    val lst = new ListBuffer[(Int,Int)]
    var topScore = 0
    for (j <- 1 to K) {
      val top = scoreOrdering.dequeue()
      lst.append(top)
      topScore = topScore + top._1
    }

    scoreOrdering.addAll(lst.toList)
    topScore
  }

  def reset(playerId: Int) : Unit = {
    scores.remove(playerId)
    val lst = new ListBuffer[(Int,Int)]
    var processed = false
    while (processed == false && scoreOrdering.isEmpty == false) {
      val top = scoreOrdering.dequeue()
      if (top._2 == playerId) {
        processed = true
      }else {
        lst.append(top)
      }
    }

    scoreOrdering.addAll(lst.toList)

  }

}