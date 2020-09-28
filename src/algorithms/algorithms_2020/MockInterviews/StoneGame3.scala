import scala.collection.mutable

case class Index(val j : Int,val a : Int,val b : Int)
object Solution {
  def stoneGameIII(A: Array[Int]): String = {


    val cache = new mutable.HashMap[Int,(Int,Int)]()
    def score(j : Int) : (Int,Int) = {
      //score of current and next Pointer
      if (j >= A.length) {
        (0,A.length)
      }else {
        if (j == A.length-1) {
          (A(j),A.length)
        }else {
          if (cache.contains(j)) {
            cache.get(j).get
          }else {
            val arr = new mutable.ArrayBuffer[List[Int]]()
            var op1 = A(j)
            val (n1, p1) = score(j + 1)
            if (p1 < A.length) {
              op1 = op1 + score(p1)._1
            }

            arr.append(List(op1, n1, j + 1))

            var o2 = 0
            if (j + 1 < A.length) {
              val (n2, p2) = score(j + 2)
              o2 = A(j) + A(j + 1)
              if (p2 < A.length) {
                o2 = o2 + score(p2)._1
              }

              arr.append(List(o2, n2, j + 2))
            }

            var o3 = 0
            if (j + 2 < A.length) {
              val (n3, p3) = score(j + 3)
              o3 = A(j) + A(j + 1) + A(j + 2)
              if (p3 < A.length) {
                o3 = o3 + score(p3)._1
              }

              arr.append(List(o3, n3, j + 3))
            }

            arr.sortInPlace()(new Ordering[List[Int]] {
              override def compare(x: List[Int], y: List[Int]): Int = {
                val d1 = x.head - x.tail.head
                val d2 = y.head - y.tail.head
                d1.compareTo(d2)
              }
            })

            val maxSum = arr.last.head
            val next = arr.last.tail.tail.head
            cache += ((j,((maxSum,next))))
            (maxSum, next)
          }
        }
      }
    }

    val (aScore,p1) = score(0)
    val bScore = A.sum - aScore
    if (aScore > bScore) {
      "Alice"
    }else {
      if (bScore > aScore) {
        "Bob"
      }else {
        "Tie"
      }
    }

  }

  def main(args: Array[String]): Unit = {
    println(stoneGameIII(Array(-1,-2,-3)))
  }
}