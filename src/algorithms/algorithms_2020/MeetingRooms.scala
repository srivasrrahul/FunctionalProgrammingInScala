object Solution {
  def canAttendMeetings(intervals: Array[Array[Int]]): Boolean = {
    //0,30 5,10 15,20
    intervals.sortInPlace()(new Ordering[Array[Int]] {
      override def compare(x: Array[Int], y: Array[Int]): Int = {
        x(0).compareTo(y(0))

      }
    })

    def isOverlap(i1 : Array[Int],i2 : Array[Int]) : Boolean = {
      i2(0) < i1(1)
    }

    var ifOverlap = false
    for (j <- 1 to intervals.length-1 if ifOverlap == false) {
      if (isOverlap(intervals(j-1),intervals(j))) {
        ifOverlap = true
      }
    }

    ifOverlap
  }
}