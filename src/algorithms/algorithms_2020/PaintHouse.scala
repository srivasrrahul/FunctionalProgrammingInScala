object Solution {
  def minCost(costs: Array[Array[Int]]): Int = {
    if (costs.length == 0) {
      0
    }else {
      var costMatrix = Array.ofDim[Int](costs.length, costs(0).length)

      costMatrix(0)(0) = costs(0)(0)
      costMatrix(0)(1) = costs(0)(1)
      costMatrix(0)(2) = costs(0)(2)

      def returnOtherColor(color: Int): (Int, Int) = {
        color match {
          case 0 => (1, 2)
          case 1 => (2, 0)
          case 2 => (0, 1)
          case _ => (-1, -1)
        }
      }

      for (j <- 1 to costs.length - 1) {
        for (k <- 0 to 2) {
          val otherColor = returnOtherColor(k)
          val cost1 = costMatrix(j - 1)(otherColor._1) + costs(j)(k)
          val cost2 = costMatrix(j - 1)(otherColor._2) + costs(j)(k)

          costMatrix(j)(k) = scala.math.min(cost1, cost2)
        }
      }


      costMatrix.last.min
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(17,2,17),Array(16,16,5),Array(14,3,19))
    println(minCost(arr))
  }
}