import scala.collection.mutable.ListBuffer
import util.control.Breaks._
import scala.collection.mutable.HashSet
class CacheKey(startIndex : Int,endIndex : Int)
object Solution {
////  def stoneGame(piles: Array[Int]): Boolean = {
////
////    //val results = new ListBuffer[(Int,Int)]()
////    val cache = new HashSet[CacheKey]
////    var results = false
////    def play(startIndex : Int,
////             endIndex : Int,
////             playerPlaying : Int,
////             playerNext : Int,
////             playerPlayingCollectedBalls : Int,
////             playerNextCollectedBalls : Int): Unit = {
////      if (startIndex > endIndex) {
////        //println("Done")
////        if (playerPlaying == 0) {
////          //results += ((playerPlayingCollectedBalls,playerNextCollectedBalls))
////          if (playerPlayingCollectedBalls > playerNextCollectedBalls) {
////            results = true
////          }
////        }else {
////          //results += ((playerNextCollectedBalls,playerPlayingCollectedBalls))
////          if (playerNextCollectedBalls > playerPlayingCollectedBalls) {
////            results = true
////          }
////        }
////      }else {
////        if (playerPlaying == 0) {
////          val cacheKey = new CacheKey(startIndex,endIndex)
////          if (cache(cacheKey) == false) {
////            play(startIndex+1,endIndex,playerNext,playerPlaying,playerNextCollectedBalls,playerPlayingCollectedBalls+piles(startIndex))
////            play(startIndex,endIndex-1,playerNext,playerPlaying,playerNextCollectedBalls,playerPlayingCollectedBalls+piles(endIndex))
////
////            //Update cache
////            if (playerPlaying == 0) {
////              cache += cacheKey
////            }
////          }else {
////            println("Cache hit")
////          }
////        }else {
////          play(startIndex+1,endIndex,playerNext,playerPlaying,playerNextCollectedBalls,playerPlayingCollectedBalls+piles(startIndex))
////          play(startIndex,endIndex-1,playerNext,playerPlaying,playerNextCollectedBalls,playerPlayingCollectedBalls+piles(endIndex))
////        }
////
////      }
////    }
////
////    play(0,piles.length-1,0,1,0,0)
//////    var res = false
//////    breakable {
//////      for (r <- results) {
//////        //println(r)
//////        if (r._1 > r._2) {
//////          res = true
//////          break
//////        }
//////      }
//////    }
//////
//////    res
////    results
////
////
////  }
//
//  def stoneGame(piles : Array[Int]) : Boolean = {
//    def itr(startIndex : Int,endIndex : Int) : Int = {
//      //println("startIndex = " + startIndex + " endIndex = " + endIndex)
//      val diff = endIndex-startIndex
//      diff match {
//        case 1 => {
//          if (piles(startIndex) >= piles(endIndex)) {
//            piles(startIndex)
//          }else {
//            piles(endIndex)
//          }
//        }
//        case _ => {
//          var maxBallsCollected = Int.MinValue
//          val ballsCollected1 = piles(startIndex) + itr(startIndex+1,endIndex-1)
//          if (ballsCollected1 > maxBallsCollected) {
//            maxBallsCollected = ballsCollected1
//          }
//
//          val ballsCollected2 = piles(startIndex) + itr(startIndex+2,endIndex)
//          if (ballsCollected2 > maxBallsCollected) {
//            maxBallsCollected = ballsCollected2
//          }
//
//          val ballsCollected3 = piles(endIndex) + itr(startIndex+1,endIndex-1)
//          if (ballsCollected3 > maxBallsCollected) {
//            maxBallsCollected = ballsCollected3
//          }
//
//          val ballsCollected4 = piles(endIndex) + itr(startIndex,endIndex-2)
//          if (ballsCollected4 > maxBallsCollected) {
//            maxBallsCollected = ballsCollected4
//          }
//
//          //println("startIndex = " + startIndex + " endIndex = " + endIndex + " maxBallsCollected = " + maxBallsCollected )
//          maxBallsCollected
//
//
//
//
//        }
//      }
//    }
//
//    val ballsCollected = itr(0,piles.length-1)
//    //println(ballsCollected)
//    val totalBalls = piles.sum
//    val pending = totalBalls - ballsCollected
//    if (pending > ballsCollected) {
//      false
//    }else {
//      true
//    }
//  }

  def stoneGame(piles : Array[Int]) : Boolean = {

    val arr = Array.ofDim[Int](piles.length,piles.length)
    def findMax(startIndex : Int,endIndex : Int) : Int = {

      val diff = endIndex - startIndex
      diff match {
        case 1 => {
          if (piles(startIndex) >= piles(endIndex)) {
            piles(startIndex)
          }else {
            piles(endIndex)
          }
        }
        case _ => {
          var maxBallsCollected = Int.MinValue
          val ballsCollected1 = piles(startIndex) + arr(startIndex+1)(endIndex-1)
          if (ballsCollected1 > maxBallsCollected) {
            maxBallsCollected = ballsCollected1
          }

          val ballsCollected2 = piles(startIndex) + arr(startIndex+2)(endIndex)
          if (ballsCollected2 > maxBallsCollected) {
            maxBallsCollected = ballsCollected2
          }

          val ballsCollected3 = piles(endIndex) + arr(startIndex+1)(endIndex-1)
          if (ballsCollected3 > maxBallsCollected) {
            maxBallsCollected = ballsCollected3
          }

          val ballsCollected4 = piles(endIndex) + arr(startIndex)(endIndex-2)
          if (ballsCollected4 > maxBallsCollected) {
            maxBallsCollected = ballsCollected4
          }

          maxBallsCollected
        }
      }

    }



    for (k <-0 to piles.length-2) {
      arr(k)(k+1) = math.max(piles(k),piles(k+1))
    }

    var maxCollected = Int.MinValue
    for (i <- 0 to piles.length-2) {
      for (k <- i+1 to piles.length-1) {
        arr(i)(k) = findMax(i,k)
        if (maxCollected < arr(i)(k)) {
          maxCollected = arr(i)(k)
        }
      }
    }

    //println(maxCollected)

    val totalBalls = piles.sum
    val pending = totalBalls - maxCollected
    if (pending > maxCollected) {
      false
    }else {
      true
    }
    true
  }

  def main(args: Array[String]): Unit = {
    println(stoneGame(Array(5,3,4,5)))

  }
}

