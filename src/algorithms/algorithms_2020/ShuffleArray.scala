import scala.util.Random

class Solution(_nums: Array[Int]) {

  val arr = new Array[Int](_nums.length)
  /** Resets the array to its original configuration and return it. */
  def reset(): Array[Int] = {
    Array.copy(_nums,0,arr,0,_nums.length)
    arr
  }

  reset()
  /** Returns a random shuffling of the array. */
  def shuffle(): Array[Int] = {
     for (j <- 0 to arr.length-1) {
       val shuffleIndex = Random.between(j,arr.length)
       val temp = arr(j)
       arr(j) = arr(shuffleIndex)
       arr(shuffleIndex) = temp
     }

    arr
  }

}