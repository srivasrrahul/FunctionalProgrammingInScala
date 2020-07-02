import scala.collection.mutable.ListBuffer

class MovingAverage(_size: Int) {

  /** Initialize your data structure here. */
  val lst = new ListBuffer[Int]
  var sum = 0

  def next(v: Int): Double = {
    if (lst.size < _size) {
      sum = sum + v
      lst.append(v)
    }else {
      sum = sum - lst.head
      lst.dropInPlace(1)
      lst.append(v)
      sum = sum + v
    }

    sum.toDouble/lst.size.toDouble
  }

}