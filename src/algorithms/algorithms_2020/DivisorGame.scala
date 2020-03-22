import scala.collection.mutable
import util.control.Breaks._


object Solution {
//  def divisorGame(N: Int): Boolean = {
//
//    def play(currentVal : Int) : Boolean = {
//      if (currentVal == 1) {
//        false
//      }else {
//        var thisPlayerResult = false
//        breakable {
//          for (j <- 1 to currentVal - 1) {
//            if (currentVal % j == 0) {
//              val otherPlayerResult = play(currentVal - j)
//              if (otherPlayerResult == false) {
//                //Current player can win
//                thisPlayerResult = true
//                break
//              }
//            }
//          }
//        }
//
//        thisPlayerResult
//      }
//    }
//
//    play(N);
//
//
//  }


  def divisorGame(N: Int): Boolean = {
    val successData = new mutable.HashSet[Int]()
    successData.add(2)

    var current = 3

    while (current <= N) {

      var success = false
      breakable {
        for (j <- 1 to current-1) {
          if (current % j == 0) {
            val p = current-j
            if (successData.contains(p) == false) {
              success = true
            }
          }
        }
      }

      if (success == true) {
        successData.add(current)
      }

      current = current + 1
    }

    successData.contains(N)



  }



  def main(args: Array[String]): Unit = {
    println(divisorGame(3))
  }
}