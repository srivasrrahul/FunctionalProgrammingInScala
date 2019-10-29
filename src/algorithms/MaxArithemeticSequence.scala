import util.control.Breaks._
object Solution {
  def util(arr: Array[Int]): Int = {
    //println("Hellop")
    def findSeqLength(lastIndex : Int,diffNeeded : Int) : Int = {
      if (lastIndex < 0) {
        0
      }else {
        val curr = arr(lastIndex)
        var found = -1
        breakable {
          for (j <- lastIndex-1  to 0 by -1) {
            if ((arr(j) + diffNeeded) == curr) {
              found = j
              break
            }
          }
        }

        //println("test")
        if (found == -1) {
          0
        } else {
          //println("Found seq length recur")
          1 + findSeqLength(found, diffNeeded)
        }
      }
    }

    arr.length match {
      case 2 => {
        //println("case 2")
        2
      }
      case 3 => {
        //println("case ")
        val d1 = arr(1) - arr(0)
        val d2 = arr(2) - arr(1)

        if (d1 == d2) {
          3
        }else {
          2
        }
      }

      case _ => {
        //println("case n")
        var maxSeqLength = 2
        for (k <- arr.length-1 to 0 by -1) {
          val last = arr(k)

          for (j <- k - 1 to 0 by -1) {
            if (arr(j) < last) {
              //println("Found less for " + j)
              val diff = last - arr(j)
              //println("Diff found " + diff)
              var seqLength = findSeqLength(j,diff)
              if (seqLength != 0) {
                seqLength = seqLength + 2
              }
              //println("Length returned for k = " + k + ",j = " + j + " => " + seqLength)
              if (seqLength > maxSeqLength) {
                maxSeqLength = seqLength
              }
            }
          }

        }

        maxSeqLength
      }
    }
  }

  def longestArithSeqLength(ints: Array[Int]) : Int = {
    math.max(util(ints),util(ints.reverse))
  }



  def main(args: Array[String]): Unit = {
    println(longestArithSeqLength(Array(20,1,15,3,10,5,8)))
    //println(longestArithSeqLength(Array(20,1,15,3,10,5,8).reverse))
  }
}