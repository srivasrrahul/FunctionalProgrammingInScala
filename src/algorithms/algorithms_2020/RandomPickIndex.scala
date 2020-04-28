import scala.collection.mutable
import scala.util.Random
import scala.util.control.Breaks._

class Solution(_nums: Array[Int]) {
  val map = new mutable.HashMap[Int,mutable.TreeSet[Int]]()
  val random = new Random(1782378)
  for (j <- 0 to _nums.length-1) {
    map.get(_nums(j)) match {
      case None => {
        val set = new mutable.TreeSet[Int]()
        set.add(j)
        map += ((_nums(j),set))
      }
      case Some(set) => {
        set.add(j)
      }
    }
  }

  //println(map)
  def pick(target: Int): Int = {
    val targetMap = map.get(target).get
    val min = targetMap.head
    val max = targetMap.last

    val randomRank = random.between(0,targetMap.size)
    //print("random rank " + randomRank + " :" + targetMap.size + ":")
    var j = 0
    var retValue = -1
    breakable {
      for (targetIndex <- targetMap) {
        if (j == randomRank) {
          retValue = targetIndex
          break()
        } else {
          j = j + 1
        }
      }
    }

    retValue
  }

  def main(args: Array[String]): Unit = {


  }

}

object Main {
  def main(args: Array[String]): Unit = {
    var arr = Array(1,1,2,2,2,3,3,3,3)
    val solution = new Solution(arr)
    println(solution.pick(1))
    println(solution.pick(1))
    println(solution.pick(1))
    println(solution.pick(1))

    println(solution.pick(2))
    println(solution.pick(2))
    println(solution.pick(2))
    println(solution.pick(2))

    println(solution.pick(3))
    println(solution.pick(3))
    println(solution.pick(3))
    println(solution.pick(3))
    println(solution.pick(3))
    println(solution.pick(3))

  }
}