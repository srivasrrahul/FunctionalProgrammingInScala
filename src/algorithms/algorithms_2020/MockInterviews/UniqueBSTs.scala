import scala.collection.mutable
case class Index(val x : Int,val y : Int)
object Solution {
  def numTrees(n: Int): Int = {
    val cache = new mutable.HashMap[Index,Int]()
    def itr(begin : Int,end : Int) : Int = {
      if (begin == end) {
        1
      }else {
        //println("Here")
        val index = new Index(begin, end)
        if (cache.contains(index)) {
          cache.get(index).get
        } else {
          var total = 0
          for (r <- begin to end) {
            var leftSubTreeCount = 0
            if (r > begin) {
              leftSubTreeCount = itr(begin, r - 1)
              //println("for " + begin + " " + (r-1) + leftSubTreeCount)
            }

            var rightSubTreeCount = 0
            if (r + 1 <= end) {
              rightSubTreeCount = itr(r + 1, end)
              //println("for " + (r+1) + " " + end + rightSubTreeCount)
            }

            //Combine each with left
            var totalCount = 0
            if (leftSubTreeCount == 0) {
              totalCount = rightSubTreeCount
            } else {
              if (rightSubTreeCount == 0) {
                totalCount = leftSubTreeCount
              } else {
                totalCount = leftSubTreeCount * rightSubTreeCount
              }
            }

            total = total + totalCount
          }

          cache += ((index,total))
          total
        }
      }
    }

    itr(1,n)
  }

  def main(args: Array[String]): Unit = {
    println(numTrees(3))
  }
}