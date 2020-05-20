object Solution {
  def smallerNumbersThanCurrent(nums: Array[Int]): Array[Int] = {
    val pq = new scala.collection.mutable.PriorityQueue[(Int,Int)]()(new Ordering[(Int,Int)] {
      def compare(x : (Int,Int),y : (Int,Int)) : Int = {
        x._1.compareTo(y._1)
      }
    })

    var j = 0
    for (num <- nums) {
      pq.addOne((num,j))
      j = j + 1
    }

    val smaller = new Array[Int](nums.length)
    while (pq.isEmpty == false) {
      val top = pq.dequeue

      //smaller(top._2) = pendingSize
      if (pq.isEmpty == false) {
        val lst = new scala.collection.mutable.ListBuffer[(Int,Int)]
        lst.append(top)
        while (pq.isEmpty == false && pq.head._1 == top._1) {
          lst.append(pq.dequeue)
          //smaller(newTop._2) = pendingSize
        }

        for (elem <- lst) {
          smaller(elem._2) = pq.size
        }
      }else {
        smaller(top._2) = pq.size
      }


    }

    smaller

  }
}