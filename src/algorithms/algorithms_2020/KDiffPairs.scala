import scala.collection.mutable

case class Value(val x : Int,val y : Int)
object Solution {
  def findPairs(nums: Array[Int], k: Int): Int = {
    val map = new mutable.HashSet[Int]()
    val solution = new mutable.HashSet[Set[Int]]()
    for (b <- nums) {
      //solve abs(a-b) = k which is (a-b) = k or -(a-b) = k a = b + k or -a+b= k -a = k-b or a = b - k
      val a1 = b + k
      val a2 = b - k

      if (map.contains(a1)) {
        solution.add(Set(a1,b))
      }

      if (map.contains(a2)) {
        solution.add(Set(a2,b))
      }

      map.add(b)

    }

    solution.size
  }

  def main(args: Array[String]): Unit = {
    println(findPairs(Array(1,2,3,4,5),-1))
  }
}