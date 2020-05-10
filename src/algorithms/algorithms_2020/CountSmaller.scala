import scala.collection.mutable

object Solution {
  def countSmaller(nums: Array[Int]): List[Int] = {
    if (nums.length == 0) {
      List()
    }else {
      val treeEncountered = new mutable.TreeMap[Int, Int]()(Ordering[Int].reverse)
      val smallerElement = Array.fill[Int](nums.length)(0)

      treeEncountered += ((nums.last, 1))
      for (j <- smallerElement.length - 2 to 0 by -1) {
        val current = nums(j)
        //println(j)

        val currentCount = treeEncountered.getOrElseUpdate(current, 0)
        treeEncountered += ((current, currentCount + 1))

        val smallerTree = treeEncountered.rangeFrom(current - 1)
        //println(treeEncountered)
        //println(" " + smallerTree)
        val countElement = smallerTree.foldRight(0)((current, accumulator) => {
          accumulator + current._2
        })

        smallerElement(j) = countElement

        //update in tree

      }

      smallerElement.toList
    }
  }

  def main(args: Array[String]): Unit = {
    println(countSmaller(Array(5,2,6,1)))
  }
}