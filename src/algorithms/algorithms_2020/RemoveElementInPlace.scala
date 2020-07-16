object Solution {
  def removeElement(nums: Array[Int], v: Int): Int = {
    var readState = 0
    var writerState = 0

    while (readState < nums.length && writerState < nums.length) {
      if (nums(readState) == v) {
        //move readstate where we can start reading next
        var k = readState+1
        //println("test1")
        while (k < nums.length && nums(k) == v) {
          //println("test")
          k = k + 1
        }

        readState = k
      }else {
        //println("test")
        nums(writerState) = nums(readState)
        writerState = writerState + 1
        readState = readState+1
      }
    }

    writerState
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(0,4,4,0,4,4,4,0,2)
    println(removeElement(arr,4))
    println(arr.mkString(","))
  }
}