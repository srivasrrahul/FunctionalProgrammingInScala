import scala.collection.mutable

object Solution {
//  def canJump(nums: Array[Int]): Boolean = {
//    val cache = new mutable.HashMap[Int,Boolean]()
//    def itr(currentIndex : Int) : Boolean = {
//      val MaxLimit = nums.length-1
//      currentIndex match {
//        case MaxLimit => {
//          true
//        }
//        case _ => {
//          cache.get(currentIndex) match {
//            case Some(value) => {
//              //println("Here")
//              value
//
//            }
//            case None => {
//              val maxJumpFromCurrent = nums(currentIndex)
//              var endReached = false
//              for (j <- 1 to maxJumpFromCurrent if endReached == false) {
//                if (currentIndex + j < nums.length) {
//                  endReached = itr(currentIndex+j)
//                }
//              }
//
//              cache += ((currentIndex,endReached))
//              endReached
//            }
//          }
//
//        }
//      }
//    }
//
//    itr(0)
//  }

  def canJump(nums: Array[Int]): Boolean = {
    val q = new mutable.Queue[Int]()
    val visited = new mutable.HashSet[Int]()

    q.addOne(0)


    var endReached = false
    var maxReached = 0
    while (q.isEmpty == false && endReached == false) {
      val top = q.dequeue()
      if (top == nums.length-1) {
        endReached = true
      }else {
        visited.add(top)
        maxReached = top
        for (j <- 1 to nums(top)) {
          val next = top + j
          if (visited.contains(next) == false && next > maxReached) {
            q.addOne(next)
            maxReached = next
          }

        }
      }
    }

    endReached
  }

  def main(args: Array[String]): Unit = {
    println(canJump(Array(8,2,4,4,4,9,5,2,5,8,8,0,8,6,9,1,1,6,3,5,1,2,6,6,0,4,8,6,0,3,2,8,7,6,5,1,7,0,3,4,8,3,5,9,0,4,0,1,0,5,9,2,0,7,0,2,1,0,8,2,5,1,2,3,9,7,4,7,0,0,1,8,5,6,7,5,1,9,9,3,5,0,7,5)))
  }
}