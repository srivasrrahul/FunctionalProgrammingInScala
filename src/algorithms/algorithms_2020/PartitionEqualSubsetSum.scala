import scala.collection.mutable

case class Key(val index : Int,val s1 : Int,val s2 : Int)
object Solution {
  def canPartition(nums: Array[Int]): Boolean = {
    val cache = new mutable.HashMap[Key,Boolean]()
    def itr(index : Int,set1 : Int,set2 : Int) : Boolean = {
      if (index == nums.length) {
        //println("Finished " + set1 + " " + set2)

        set1 == set2
      }else {
        //two options
        val key = new Key(index,set1,set2)
        if (cache.contains(key)) {
          //println("Cache hit")
          cache.get(key).get
        }else {
          val retValue = itr(index+1,nums(index)+set1,set2) || itr(index+1,set1,nums(index)+set2)
          cache += ((key,retValue))
          retValue
        }

      }
    }

    itr(0,0,0)
  }

  def main(args: Array[String]): Unit = {
    println(canPartition(Array(1, 5,11,5)))
  }
}