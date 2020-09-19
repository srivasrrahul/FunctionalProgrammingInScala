object Solution {
  def countTriplets(A: Array[Int]): Int = {
    var count = 0
    for (j <- 0 to A.length-1) {
      for (k <- 0 to A.length-1) {
        for (p <- 0 to A.length-1) {
          if ((A(j) & A(k) & A(p)) == 0) {
            count = count +1
          }
        }
      }
    }

    count
  }
}