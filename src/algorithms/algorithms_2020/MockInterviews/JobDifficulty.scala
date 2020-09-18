
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
object Solution {
  def minDifficulty(jobDifficulty: Array[Int], d: Int): Int = {
    val cache = new mutable.HashMap[Index,Option[Int]]()
    def itr(dayIndex : Int,jobIndex : Int) : Option[Int] = {
      if (dayIndex < 1 && jobIndex >= 0) {
        None
      }else {
        if (dayIndex == 1) {
          var maxDiff = Int.MinValue
          for (j <- 0 to jobIndex) {
            if (jobDifficulty(j) > maxDiff) {
              maxDiff = jobDifficulty(j)
            }
          }
          if (jobIndex < 0) {
            None
          } else {
            Some(maxDiff)
          }
        } else {
          val index = new Index(dayIndex, jobIndex)
          if (cache.contains(index)) {
            cache.get(index).get
          } else {
            val lstBuffer = new ListBuffer[Int]
            var maxDiff = jobDifficulty(jobIndex)
            for (j <- jobIndex - 1 to 0 by -1) {
              //we'll schedule job from index jobIndex to j+1

              if (jobDifficulty(j + 1) > maxDiff) {
                maxDiff = jobDifficulty(j + 1)
              }

              val pLst = itr(dayIndex - 1, j)

              if (pLst.isDefined) {
                lstBuffer.append(maxDiff + pLst.get)
              }
            }

            if (lstBuffer.size == 0) {
              cache += ((index,None))
              None
            } else {
              val retValue = lstBuffer.min
              cache += ((index,Some(retValue)))
              Some(retValue)
            }
          }
        }
      }
    }

    val lst = itr(d,jobDifficulty.length-1)
    //println(lst)
    if (lst.isDefined) {
      lst.get
    }else {
      -1
    }
  }
}

//import scala.collection.mutable.ListBuffer
//
//object Solution {
//  def minDifficulty(jobDifficulty: Array[Int], d: Int): Int = {
//    val dp = Array.ofDim[Option[List[Int]]](d,jobDifficulty.length)
//    for (j <- 0 to d-1) {
//      for (k <- 0 to jobDifficulty.length-1) {
//        dp(j)(k) = None
//      }
//    }
//
//    dp(0)(0) = Some(List(jobDifficulty(0)))
//
//    var maxDifficulty =  jobDifficulty(0)
//    for (k <- 0 to jobDifficulty.length-1 if jobDifficulty.length-k >=d) {
//      maxDifficulty = math.max(maxDifficulty,jobDifficulty(k))
//      dp(0)(k) = Some(List(maxDifficulty))
//    }
//
//
//    for (j <- 1 to d-1) {
//      //on day d schedule from j to end
//      for (k <- j to jobDifficulty.length-1) {
//        val lst = new ListBuffer[Int]
//        maxDifficulty = jobDifficulty(k) //k is handled
//        for (p <- k to 0 by -1) {
//          if (p >= j) {
//            //days have to be greater than task
//            if (dp(j - 1)(p).isDefined) {
//              for (prevVal <- dp(j - 1)(p).get) {
//                lst.append(maxDifficulty + prevVal)
//              }
//            }
//          }
//          maxDifficulty = math.max(jobDifficulty(p), maxDifficulty)
//        }
//
//        if (lst.isEmpty == false) {
//          dp(j)(k) = Some(lst.toList)
//        }
//      }
//    }
//
//    for (j <- 0 to d-1) {
//      println(dp(j).mkString(","))
//    }
//
//    println(dp.last.last.get.min)
//    0
//  }
//
//  def main(args: Array[String]): Unit = {
//    println(minDifficulty(Array(11,111,22,222,33,333,44,444),6))
//  }
//}