import scala.collection.Searching

object Solution {
  def search(nums: Array[Int], target: Int): Boolean = {
    def itr(begin : Int,end : Int) : Boolean = {
      if (begin == end) {
        nums(begin) == target
      }else {
        val mid = begin + (end-begin)/2
        if (nums(mid) == target) {
          true
        }else {

          if (nums(begin) >= nums(end)) {
            if (nums(mid) >= nums(begin)) {
              println("here0 for " + begin + " " + mid + " " + end + " " + target)
              var found = false
              //left one is reverse sorted and if not found focus recursively on right
              nums.search(target,begin,mid+1)match {
                case Searching.Found(_) => found = true
                case _ => found = false
              }

              if (found == false) {
                itr(mid+1,end)
              }else {
                found
              }
            }else {
              //right one is sorted and if not found recursively on left
              println("here for " + begin + " " + mid + " " + end + " " + target)
              var found = false
              //println(nums.search(target,mid+1,end+1))
              nums.search(target,mid+1,end+1) match {
                case Searching.Found(_) => found = true
                case _ => found = false
              }

              if (found == false) {
                itr(begin,mid)
              }else {
                found
              }
            }
          }else {
            println("here")
            nums.search(target,begin,end+1) match {
              case Searching.Found(_) => true
              case _ => false
            }
          }
        }
      }
    }

    if (nums.isEmpty) {
      false
    }else {
      itr(0,nums.length-1)
    }

  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1)
    println(arr.mkString(","))
    println(search(arr,2))
  }
}