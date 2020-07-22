object Solution {
  def sumOfDistancesInTree(N: Int, edges: Array[Array[Int]]): Array[Int] = {
    val dist = Array.ofDim[Int](N,N)

    for (j <- 0 to N-1) {
      for (k <- 0 to N-1) {
        dist(j)(k) = Int.MaxValue
      }
    }
    for (j <- 0 to N-1) {
      dist(j)(j) = 0
    }

    for (edge <- edges) {
      val u = edge(0)
      val v = edge(1)
      dist(u)(v) = 1
      dist(v)(u) = 1
    }

    for (k <- 0 to N-1) {
      for (i <- 0 to N-1) {
        for (j <- 0 to N-1) {
          val distanceFromIToK = dist(i)(k)
          val distanceFromKToJ = dist(k)(j)
          if (distanceFromIToK != Int.MaxValue && distanceFromKToJ != Int.MaxValue) {
            if (dist(i)(j) > (distanceFromIToK + distanceFromKToJ)) {
              dist(i)(j) = distanceFromIToK + distanceFromKToJ
            }
          }
        }
      }
    }

    val retValue = new Array[Int](N)

    for (j <- 0 to retValue.length-1) {
      retValue(j) = dist(j).sum
    }

    retValue


  }
}