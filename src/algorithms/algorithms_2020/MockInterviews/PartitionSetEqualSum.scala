import scala.collection.mutable

case class Index(val index : Int,val s1 : Int,val s2 : Int)
object Solution {
  def canPartition(nums: Array[Int]): Boolean = {
    //var ifTrueFound = false
    val cache = new mutable.HashMap[Index,Boolean]
    def itr(currentIndex : Int,s1 : Int,s2 : Int) : Boolean = {

      if (currentIndex == 0) {
        var retValue = false

        val opt1 = s1+ nums(currentIndex)
        val opt2 = s2 + nums(currentIndex)
        if (opt1 == s2 || s1 == opt2) {
          //ifTrueFound = true
          retValue = true
        }

        retValue
      }else {
        val index1 = new Index(currentIndex,s1,s2)
        val index2 = new Index(currentIndex,s2,s1)
        if (cache.contains(index1) || cache.contains(index2)) {
          val r1 = cache.getOrElse(index1,false)
          val r2 = cache.getOrElse(index2,false)
          r1 || r2
        }else {

          val r1 = itr(currentIndex - 1, s1 + nums(currentIndex), s2)
          val r2 = itr(currentIndex - 1, s1, s2 + nums(currentIndex))

          cache += ((new Index(currentIndex,s1,s2),r1 || r2))
          cache += ((new Index(currentIndex,s2,s1),r1 || r2))

          r1 || r2
        }
      }
    }

    itr(nums.length-1,0,0)
    //ifTrueFound
  }

  def main(args: Array[String]): Unit = {
    println(canPartition(Array(1, 2, 3, 5)))

  }
}