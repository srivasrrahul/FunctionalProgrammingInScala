import scala.collection.mutable

object Solution {
  def isAlienSorted(words: Array[String], order: String): Boolean = {
    val orderMap = new mutable.HashMap[Char,Int]()
    for (j <- 0 to order.length-1) {
      orderMap += ((order(j),j))
    }
    object AlienOrdering extends Ordering[Char] {
      override def compare(x: Char, y: Char): Int = {
        val o1 = orderMap.get(x).get
        val o2 = orderMap.get(y).get
        o1.compareTo(o2)
      }
    }

    object StringOrdering extends Ordering[String] {
      override def compare(x: String, y: String): Int = {
        var cmpValue = 0
        for (j <- 0 to math.min(x.length,y.length)-1 if cmpValue == 0) {
          cmpValue = AlienOrdering.compare(x(j),y(j))
        }

        if (cmpValue == 0) {
          x.length.compareTo(y.length)
        }else {
          cmpValue
        }
      }
    }

    val newSorted = words.sorted(StringOrdering)
    var isSorted = true
    for (j <- 0 to newSorted.length-1 if isSorted == true) {
      if (newSorted(j) != words(j)) {
        isSorted = false
      }
    }

    isSorted
  }
}