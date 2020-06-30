import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Solution {
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val arrBuffer = new ArrayBuffer[Int]()
    for (j <- 0 to n-1) {
      arrBuffer.append(nums(j))
      arrBuffer.append(nums(n+j))
    }

    arrBuffer.toArray
  }

  def main(args: Array[String]): Unit = {
    print(shuffle(Array(2,5,1,3,4,7),3).mkString(","))
  }
}