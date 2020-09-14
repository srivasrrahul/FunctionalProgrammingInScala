import scala.collection.mutable

case class Index(val used : List[Int],val unused : List[Int])
object Solution {
  def maxSatisfaction(satisfaction: Array[Int]): Int = {
    //visited in this order
    val cache = new mutable.HashMap[Index,Int]()
    def itr(used : List[Int],discarded : List[Int]) : Int = {
      if (satisfaction.length == (used.length + discarded.length)) {
        var j = 1
        var likes = 0
        for (useIndex <- used.reverse) {
          likes = likes + j*satisfaction(useIndex)
          j = j + 1
        }

        likes
      }else {
        val index = new Index(used,discarded)
        if (cache.contains(index)) {
          //println("cache hit")
          cache.get(index).get
        }else {
          var globalMax = Int.MinValue
          for (p <- 0 to satisfaction.length - 1) {
            //two choices
            if (used.contains(p) == false && discarded.contains(p) == false) {
              //two choices
              //choice-1
              if (satisfaction(p) < 0) {
                val option1 = itr(p :: used, discarded)
                val option2 = itr(used, p :: discarded)
                val maxOption = math.max(option1, option2)
                if (maxOption > globalMax) {
                  globalMax = maxOption
                }
              }else {
                val maxOption = itr(p :: used, discarded)
                if (maxOption > globalMax) {
                  globalMax = maxOption
                }
              }

            }
          }

          cache += ((index,globalMax))
          globalMax
        }
      }
    }

    itr(List(),List())
  }

  def main(args: Array[String]): Unit = {
    println(maxSatisfaction(Array(-1,-8,0,5,-9)))
  }
}