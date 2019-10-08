import scala.collection.mutable.HashMap
case class Index(a : Int,b : Int)


object Solution {


  def longestCommonSubsequence(text1: String, text2: String): Int = {
    val matrix = Array.ofDim[Int](text1.length,text2.length)
    if (text1.charAt(0) == text2.charAt(0)) {
      matrix(0)(0) = 1
    }else {
      matrix(0)(0) = 0
    }

    for (j <- 1 to text2.length-1) {

      if (text1.charAt(0) == text2.charAt(j)) {
        matrix(0)(j) = 1
      }else {
        matrix(0)(j) = matrix(0)(j-1)
        //0
      }
    }

    for (j <- 1 to text1.length-1) {

      if (text1.charAt(j) == text2.charAt(0)) {
        matrix(j)(0) = 1
      }else {
        matrix(j)(0) = matrix(j-1)(0)
        //0
      }
    }


    var max_val = 0
    for (j <- 1 to text1.length-1) {
      for (k <- 1 to text2.length-1) {
        if (text1.charAt(j) == text2.charAt(k)) {
          matrix(j)(k) = 1 + matrix(j-1)(k-1)
        }else {
          matrix(j)(k) = math.max(matrix(j)(k-1),matrix(j-1)(k))
        }

        if (matrix(j)(k) > max_val) {
          max_val = matrix(j)(k)
        }
      }
    }





//    print("  ")
//    println(text2.mkString(","))
//    for (j <- 0 to text1.length-1) {
//      for (k <- 0 to text2.length-1) {
//        if (k == 0) {
//          print(text1.charAt(j) + ",")
//        }
//
//        print(matrix(j)(k) + ",")
//
//      }
//      println()
//    }

    max_val
    //0



  }

//  def longestCommonSubsequence(text1: String, text2: String): Int = {
//    val cache = new HashMap[Index,Int]()
//    def itr(a : Int,b : Int) : Int = {
//      if (a >= text1.length || b >= text2.length) {
//        0
//      }else {
//        val index = Index(a,b)
//        cache.get(index) match  {
//          case Some(r) =>  {
//            //println("Caceh hit")
//            r
//          }
//          case None => {
//            val opt1 = itr(a+1,b)
//            val opt2 = itr(a,b+1)
//            if (text1.charAt(a) == text2.charAt(b)) {
//              val opt3 = 1 + itr(a+1,b+1)
//              val res = math.max(opt3,math.max(opt1,opt2))
//              cache += (Index(a,b) -> res)
//              res
//            }else {
//              val res = math.max(opt1,opt2)
//              cache += (Index(a,b) -> res)
//              res
//            }
//          }
//        }
//
//      }
//    }
//
//    itr(0,0)
//  }

  def main(args: Array[String]): Unit = {
    println(longestCommonSubsequence("abc","def"))
  }
}