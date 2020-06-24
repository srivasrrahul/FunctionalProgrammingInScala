object Solution {
  def minimumAbsDifference(arr: Array[Int]): List[List[Int]] = {
    val arrIndex = new Array[(Int,Int)](arr.length)
    for (j <- 0 to arr.length-1) {
      arrIndex(j) = ((arr(j),j))
    }

    arrIndex.sortInPlace()(new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._1.compareTo(y._1)
      }
    })

    var minDiff = Int.MaxValue
    for (j <- 1 to arrIndex.length-1) {
      val diff = math.abs(arrIndex(j)._1-arrIndex(j-1)._1)
      if (diff < minDiff) {
        minDiff = diff
      }
    }

    val resultArr = new scala.collection.mutable.ArrayBuffer[List[Int]]()
    for (j <- 1 to arrIndex.length-1) {
      val diff = math.abs(arrIndex(j)._1-arrIndex(j-1)._1)
      if (diff == minDiff) {
        if (arrIndex(j-1)._1 < arrIndex(j)._1) {
          resultArr.append(List(arrIndex(j-1)._1,arrIndex(j)._1))
        }else {
          resultArr.append(List(arrIndex(j)._1,arrIndex(j-1)._1))
        }
      }
    }

    resultArr.sortInPlace()(new Ordering[List[Int]] {
      override def compare(x: List[Int], y: List[Int]): Int = {
        x.head.compareTo(y.head)
      }
    })

    resultArr.toList
  }

  def main(args: Array[String]): Unit = {
    println(minimumAbsDifference(Array(3,8,-10,23,19,-4,-14,27)))
  }
}