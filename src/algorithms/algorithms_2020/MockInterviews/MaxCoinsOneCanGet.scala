import scala.collection.mutable

case class Index(val set : Set[Int])
object Solution {
  def maxCoins(piles: Array[Int]): Int = {
    piles.sortInPlace()(Ordering[Int].reverse)
    var j = 0
    var k = piles.length-1
    var total = 0
    while (j+2 <= k) {
      total = total + piles(j+1)
      j = j + 2
      k = k -1
    }

    total
  }

  def main(args: Array[String]): Unit = {
    println(maxCoins(Array(2,4,1,2,7,8)))
  }

}

