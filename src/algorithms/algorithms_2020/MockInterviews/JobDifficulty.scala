import scala.collection.mutable.ListBuffer

object Solution {
  def minDifficulty(jobDifficulty: Array[Int], d: Int): Int = {
    def itr(dayIndex : Int,jobIndex : Int) : Option[List[Int]] = {
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
            Some(List(maxDiff))
          }
        } else {
          val lstBuffer = new ListBuffer[Int]
          var maxDiff = jobDifficulty(jobIndex)
          for (j <- jobIndex-1 to 0 by -1) {
            //we'll schedule job from index jobIndex to j+1

            if (jobDifficulty(j+1) > maxDiff) {
              maxDiff = jobDifficulty(j+1)
            }

            val pLst = itr(dayIndex - 1, j)

            if (pLst.isDefined) {
              for (diffOption <- pLst.get) {
                lstBuffer.append(maxDiff+diffOption)
              }
            }
          }

          if (lstBuffer.size == 0) {
            None
          }else {
            Some(lstBuffer.toList)
          }
        }
      }
    }

    val lst = itr(d,jobDifficulty.length-1)
    //println(lst)
    if (lst.isDefined) {
      lst.get.min
    }else {
      -1
    }
  }
}