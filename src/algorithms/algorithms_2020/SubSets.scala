import scala.collection.mutable


object Solution {
  //[1,2,2] => [2],[] -> [2,2],[2],[2],[] -> [1,2,2],[1,2],[1,2],[1],[2,2],[2],[2],[]
  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    def subSet(index : Int) : List[List[Int]] = {
      if (index == nums.length-1) {
        List(List(nums(index)),List())
      }else {
        val retValue = new mutable.HashSet[List[Int]]()
        val nextSubSet =  subSet(index+1)
        nextSubSet.foreach(lst => {
          val updatedValue = (nums(index)::lst).sortWith(_ < _)
          retValue.add(updatedValue)
        })

        retValue.addAll(nextSubSet)
        retValue.toList
      }
    }

    subSet(0)
  }

  def main(args: Array[String]): Unit = {
    println(subsetsWithDup(Array(4,4,4,1,4)))
  }
}