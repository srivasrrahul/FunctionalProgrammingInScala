object Solution {
  def singleNumber(nums: Array[Int]): Int = {
    val countInt = new scala.collection.mutable.HashMap[Int,Int]
    for (num <- nums) {
      countInt.get(num) match {
        case None => {
          countInt += ((num,1))
        }
        case Some(alreadyExist) => {
          countInt.remove(num)
        }
      }
    }

    countInt.head._1


  }
}