import scala.collection.mutable

case class Index(val i : Int,val j : Int,val k : Int)
object Solution {
  def maximizeSweetness(sweetness: Array[Int], K: Int): Int = {
    def minSweet(j : Int, k : Int) : Int = {
      var minSweetVal = sweetness(j)
      for (i <- j to k) {
        if (sweetness(i) < minSweetVal) {
          minSweetVal = sweetness(i)
        }
      }

      minSweetVal
    }

    val sums = new Array[Int](sweetness.length)
    sums(0) = sweetness(0)
    for (j <- 1 to sweetness.length-1) {
      sums(j) = sums(j-1) + sweetness(j)
    }

    def getSums(j : Int,k : Int) : Int = {
      var retValue = sums(k)
      if (j > 0) {
        retValue = retValue - sums(j-1)
      }

      retValue
    }


    val cache = new mutable.HashMap[Index,Int]()
    def itr(i : Int,j : Int,k : Int) : Int = {
      if (k == 0) {
        //No cuts necessary
        getSums(i,j)
      }else {
        val len = j-i+1
        if (len <= k || k == -1) {
          //println("here")
          -1
        }else {

          val index = new Index(i,j,k)
          if (cache.contains(index)) {
            cache.get(index).get
          }else {
            var retValue = -1
            for (p <- i to (j - 1)) {
              //Left one size
              val leftSize = getSums(i,p)
              val pending = itr(p+1,j,k-1)
              if (pending != -1) {
                val localMin = math.min(leftSize,pending)
                if (localMin > retValue) {
                  retValue = localMin
                }
              }
            }

            //println(i + " " + j + " " + k + " : " + retValue)
            cache += ((index,retValue))
            retValue
          }

        }
      }

    }

    val minSweetVal = itr(0,sweetness.length-1,K)
    minSweetVal

  }

  def main(args: Array[String]): Unit = {
    println(maximizeSweetness(Array(1,2,2,1,2,2,1,2,2),2))
  }
}