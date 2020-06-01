import scala.collection.mutable

object Solution {

  def twoSum(nums : Array[Int],target : Int) : Array[Int] = {
    val numCount = new mutable.HashMap[Int,Set[Int]]()
    for (j <- 0 to nums.length-1) {
      val num = nums(j)
      val defaultLst = numCount.getOrElseUpdate(num,Set())
      numCount += ((num,defaultLst.+(j)))
    }


    var found = false
    var arr = new Array[Int](2)
    for (j <- 0 to nums.length-1 if found == false) {
      val num = nums(j)
      val diff = target - num

      numCount.get(diff) match {
        case None => {

        }
        case Some(set) => {
          if (diff == num) {
            if (set.size > 1) {
              arr(0) = j
              arr(1) = set.-(j).head
              found = true
            }
          }else {
            arr(0) = j
            arr(1) = set.head
            found = true
          }

        }
      }
    }

    arr
  }



  def main(args: Array[String]): Unit = {
    println(twoSum(Array(),26).mkString(","))
  }
}