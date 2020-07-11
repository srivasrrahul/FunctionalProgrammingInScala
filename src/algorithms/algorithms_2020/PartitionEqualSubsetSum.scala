object Solution {
  def canPartition(nums: Array[Int]): Boolean = {
    def itr(index : Int,set1 : List[Int],set2 : List[Int]) : Boolean = {
      if (index == nums.length) {
        //println("Finished " + set1 + " " + set2)

        set1.sum == set2.sum
      }else {
        //two options
        itr(index+1,nums(index)::set1,set2) || itr(index+1,set1,nums(index)::set2)
      }
    }

    itr(0,List(),List())
  }

  def main(args: Array[String]): Unit = {
    println(canPartition(Array(1, 2, 3, 5)))
  }
}