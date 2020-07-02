import scala.collection.mutable

object Solution {
  def findTheLongestSubstring(s: String): Int = {
    //eleetminicoworoep
    val charSet = new mutable.HashMap[Char,mutable.TreeSet[Int]]()
    for (j <- 0 to s.length-1) {
      val ch = s(j)
      ch match {
        case 'a' | 'e' | 'i' | 'o' | 'u' => {
          charSet.get(ch) match {
            case None => {
              val treeSet = new mutable.TreeSet[Int]()
              treeSet.add(j)
              charSet += ((ch,treeSet))
            }
            case Some(treeSet) => {
              treeSet.add(j)
            }
          }
        }
        case _ => {

        }
      }
    }

    //println(charSet)

    if (charSet.size == 0) {
      //base case no vowels all length
      s.length
    }else {
      var maxSize = Int.MinValue
      for (j <- 0 to s.length-1) {
        for (k <- j to s.length-1) {
          var isEven = true
          for ((_,set) <- charSet if isEven == true) {
            val lowest = set.range(j,k+1)
            if (lowest.size % 2 != 0) {
              isEven = false
            }
          }

          if (isEven == true) {
            val diff = k-j+1
            if (diff > maxSize) {
              maxSize = diff
            }
          }
        }
      }

      maxSize


    }
  }

  def main(args: Array[String]): Unit = {
    println(findTheLongestSubstring("eleetminicoworoep"))
  }
}