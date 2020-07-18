import scala.collection.mutable

case class Id(val stoneIndex : Int,val lastJumpSize : Int)
object Solution {
  def canCross(stones: Array[Int]): Boolean = {
    val stoneMap = new mutable.HashMap[Int,Int]()
    for (j <- 0 to stones.length-1) {
      stoneMap += ((stones(j),j))
    }
    //println(stoneMap)
    val cache = new mutable.HashMap[Id,Boolean]()
    def itr(stoneIndex : Int,lastJumpSize : Int) : Boolean = {
      //println(stoneIndex)
      if (stoneIndex == stones.length-1) {
        true
      }else {
        val id = new Id(stoneIndex, lastJumpSize)
        if (cache.contains(id)) {
          //println("last cache possible")
          cache.get(id).get
        } else {
          val nextJumpSizes = Array(lastJumpSize - 1, lastJumpSize, lastJumpSize + 1)
          var isLastPossible = false
          var currentPosition = stones(stoneIndex)
          for (nextJumpSize <- nextJumpSizes if isLastPossible == false) {
            val nextPossibleStoneValue = currentPosition + nextJumpSize
            if (nextPossibleStoneValue > currentPosition && stoneMap.contains(nextPossibleStoneValue)) {
              val localReturn = itr(stoneMap.get(nextPossibleStoneValue).get, nextJumpSize)
              if (localReturn) {
                isLastPossible = true
              }
            }
          }

          cache += ((id,isLastPossible))
          isLastPossible
        }
      }
    }

    itr(0,0)
  }

  def main(args: Array[String]): Unit = {
    println(canCross(Array(0,1,2,3,4,8,9,11)))
  }
}