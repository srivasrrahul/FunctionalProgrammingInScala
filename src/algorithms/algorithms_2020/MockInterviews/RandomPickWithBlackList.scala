import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class Solution(_N: Int, _blacklist: Array[Int]) {
  val validRanges = new mutable.ArrayBuffer[Range]
  var first = 0
  _blacklist.sortInPlace()
  for (j <- 0 to _blacklist.length-1) {
    if (_blacklist(j) > first) {
      validRanges.append(Range(first,_blacklist(j)))
    }

    first = _blacklist(j)+1
  }
  if (_N > first)
    validRanges.append(Range(first,_N))

  //println("Total range " + validRanges.toArray.mkString(","))
  val random = new Random(123)
  def pick(): Int = {
    val size = validRanges.size
    //println(size)
    val randomIndex = random.between(0,size)

    val randomRange = validRanges(randomIndex)
    random.between(randomRange.head,randomRange.end)
  }

}

/**
 * Your Solution object will be instantiated and called as such:
 * var obj = new Solution(N, blacklist)
 * var param_1 = obj.pick()
 */