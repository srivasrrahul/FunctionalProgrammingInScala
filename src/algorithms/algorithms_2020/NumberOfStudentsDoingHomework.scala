object Solution {
  def busyStudent(startTime: Array[Int], endTime: Array[Int], queryTime: Int): Int = {
    var count = 0
    for (j <- 0 to startTime.length-1) {
      val range = Range(startTime(j),endTime(j)+1)
      if (range.contains(queryTime)) {
        count = count + 1
      }
    }

    count
  }
}