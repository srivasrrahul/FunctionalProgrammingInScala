//itr(2) => itr(1)
//for (j <- 1 to 1)
// pLen = 2*(2-1) = 2
// for (k<- 0 to 2)
// for (p <- k+1 to 2)
// k = 0,p=0 k=0,p=1 k=0,p=2
// k=1

object Solution {
  def countOrders(N: Int): Int = {
    def itr(n : Int) : Long = {
      if (n == 1) {
        1
      }else {
        val pCount = itr(n-1)
        var count : Long = 0
        //for (j <- 1 to pCount) {
          //its list of of 2*(n-1) length
          //insert PN and DN at two places
          val pLen = 2*(n-1)
          for (k <- 0 to pLen) {
            //We insert PN at kth position
            //And DN at any of k+1st position
//            for (p <- k to pLen) {
//              count = count + 1
//            }
            count = count + (pLen-k+1)
          }
        //}
        count = count * pCount

        count % (Math.pow(10,9)+7).toLong
      }
    }

    itr(N).toInt
  }

  def main(args: Array[String]): Unit = {
    println(countOrders(20))
  }
}