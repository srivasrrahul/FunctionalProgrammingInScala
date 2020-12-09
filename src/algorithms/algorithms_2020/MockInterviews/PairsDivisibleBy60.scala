import scala.collection.mutable

object Solution {
  def numPairsDivisibleBy60(time: Array[Int]): Int = {
    val pairs = new mutable.HashMap[Int,List[Int]]()
    var count = 0

    for (t <- time) {
      val modT = t % 60
      val diff = 60-modT
      if (pairs.contains(diff)) {
        count = count + pairs.get(diff).get.size
      }else {
        if (pairs.contains(-modT)) {
          count = count + pairs.get(-modT).get.size
        }
      }
      val defLst = pairs.getOrElseUpdate(modT,List())
      pairs += ((modT,modT::defLst))
    }


    count

  }
}