import scala.collection.mutable

class RecentCounter() {

  var counter = new mutable.TreeSet[Int]()
  def ping(t: Int): Int = {
    counter.add(t)
    counter = counter.rangeFrom(t-3000)
    counter.size
  }

}
