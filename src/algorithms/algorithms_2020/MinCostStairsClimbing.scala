object Solution {
  def minCostClimbingStairs(cost: Array[Int]): Int = {
    val moneyPaid = new Array[Int](cost.size)
    moneyPaid(0) = cost(0)
    moneyPaid(1) = cost(1)

    for (j <- 2 to cost.size-1) {
      //take prev cost + optimal cost of reaching there
      val option1 = cost(j) + moneyPaid(j-1)
      val option2 = cost(j) + moneyPaid(j-2)
      moneyPaid(j) = scala.math.min(option1,option2)
    }

    //println(moneyPaid.mkString(","))
    scala.math.min(moneyPaid(cost.size-1),moneyPaid(cost.size-2))

  }

  def main(args: Array[String]): Unit = {
    println(minCostClimbingStairs(Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1)))
  }
}