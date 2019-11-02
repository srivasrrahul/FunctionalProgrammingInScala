object Solution {
//  def uniquePaths(m: Int, n: Int): Int = {
//    def itr(x : Int,y : Int) : Int = {
//      x match {
//        case t if t == m => {
//          y match {
//            case u if u == n => {
//              1
//            }
//            case _ => {
//              1
//            }
//          }
//        }
//        case _ => {
//          y match {
//            case t if t == n => {
//              1
//            }
//            case _ => {
//              itr(x+1,y) + itr(x,y+1)
//            }
//          }
//        }
//      }
//    }
//
//    itr(1,1)
//  }


  def uniquePaths(m : Int,n : Int) : Int = {
    val matrix = Array.ofDim[Int](m,n)
    for (j <- 0 to n-1) {
      matrix(m-1)(j) = 1
    }

    for (k <- 0 to m-1) {
      matrix(k)(n-1) = 1
    }

    for (j <- m-2 to 0 by -1) {
      for (k <- n-2 to 0 by -1) {
        matrix(j)(k) = matrix(j+1)(k) + matrix(j)(k+1)
      }
    }

    matrix(0)(0)

  }



  def main(args: Array[String]): Unit = {
    println(uniquePaths(1,1))
  }
}