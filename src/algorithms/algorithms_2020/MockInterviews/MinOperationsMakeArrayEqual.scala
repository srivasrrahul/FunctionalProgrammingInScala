object Solution {
  def minOperations(n: Int): Int = {
    val arr = new Array[Int](n)
    for (j <- 0 to n-1) {
      arr(j) = 2*(j+1)+1
    }

    var target = 0
    if (n %2 == 0) {
      target = (arr.head + arr.last)/2
    }else {
      target = arr(n/2)
    }

    var cost = 0
    for (j <- 0 to (n/2)-1) {
      cost = cost + (target-arr(j))
    }

    cost


  }
}