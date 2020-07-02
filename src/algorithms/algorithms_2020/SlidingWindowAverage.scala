import scala.collection.mutable.ListBuffer

class MovingAverage(_size: Int) {

  /** Initialize your data structure here. */
  val lst = new ListBuffer[Int]

  def next(v: Int): Double = {
    if (lst.size < _size) {
      lst.append(v)
    }else {
      lst.dropInPlace(1)
      lst.append(v)
    }

    lst.sum.toDouble/lst.size.toDouble
  }

}