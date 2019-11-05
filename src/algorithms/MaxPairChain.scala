import scala.collection.mutable

object Solution {
  def isValidPair(x : Array[Int],y : Array[Int]) : Boolean = {
    x(1) < y(0)
  }

  def printArr(x : Array[Array[Int]]) : Unit = {
    for (i <- x) {
      print(i.mkString(":"))
      print(" ,")
    }

    println()
  }
  def findLongestChain(pairs: Array[Array[Int]]): Int = {
//    scala.util.Sorting.quickSort(pairs)(new Ordering[Array[Int]] {
//      override def compare(x: Array[Int], y: Array[Int]): Int = {
//        y(1).compareTo(x(0))
//      }
//    })
    //pairs = pairs.reverse

    val cache = new mutable.HashMap[Int,Int]()
     def itr(index : Int) : Int = {
       cache.get(index) match {
         case None => {
           var count = 1
           for (x <- 0 to pairs.length-1) {
             if (index != x && isValidPair(pairs(x),pairs(index)))  {
               val countReturned = itr(x)
               if (countReturned + 1 > count) {
                 count = countReturned + 1
               }
             }


           }

           cache += (index -> count)
           count
         }
         case Some(v) => {
           v
         }
       }

     }

    var count = 1
    for (j <- 0 to pairs.length-1) {
      val localCount = itr(j)
      if (localCount > count) {
        count = localCount
      }
    }

    count
  }

//  def dp(pairs: Array[Array[Int]]) : Int = {
//    val matrix = Array.ofDim[Int](pairs.length,pairs.length)
//    for (j <- 0 to pairs.length-1) {
//      for (k <- 0 to pairs.length-1) {
//        if (j != k) {
//          matrix(j)(k) = 0
//        }else {
//          matrix(j)(k) = 1
//        }
//      }
//    }
//
//
//
//
//
//  }

  def main(args: Array[String]): Unit = {
    //[[3,10],[3,7],[7,10],[7,9],[-1,7],[-9,5],[2,8]]
    //[-9,5],[2,8]
    //println(findLongestChain(Array(Array(1,2), Array(2,3), Array(3,4))))
    println(findLongestChain(Array(Array(3,10), Array(3,7), Array(7,10),Array(7,9),Array(-1,7),Array(-9,5),Array(3,4))))
    //println(dp())
  }
}