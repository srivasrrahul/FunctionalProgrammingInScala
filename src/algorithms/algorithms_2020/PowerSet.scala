import scala.collection.mutable.ListBuffer

object Solution {
  def subsets(nums: Array[Int]): List[List[Int]] = {
    def powerSet(index : Int ) : List[List[Int]] = {
      if (index == nums.length-1) {
        List(List(nums(index)),List())
      }else {
        val restPowerSet = powerSet(index+1)
        val retValue = new ListBuffer[List[Int]]
        restPowerSet.foreach(lst => {
          retValue.addOne(nums(index)::lst)
        })

        retValue.addAll(restPowerSet)
        retValue.toList
      }
    }

    powerSet(0)
  }

  def main(args: Array[String]): Unit = {
    println(subsets(Array(1,2,3)).mkString("\n"))
  }
}