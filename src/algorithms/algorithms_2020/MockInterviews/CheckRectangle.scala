object Solution {
  def isRectangleOverlap(rec1: Array[Int], rec2: Array[Int]): Boolean = {
    var retValue = true
    if (rec1(2) <= rec2(0) || rec2(2) <= rec1(0)) {
      retValue = false
    }

    if (rec1(3) <= rec2(1) || rec2(3) <= rec1(1)) {
      retValue = false
    }

    retValue
  }
}