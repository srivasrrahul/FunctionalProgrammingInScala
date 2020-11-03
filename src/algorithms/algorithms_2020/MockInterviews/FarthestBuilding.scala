object Solution {
  def furthestBuilding(heights: Array[Int], bricks: Int, ladders: Int): Int = {
    def itr(index : Int,pendingBricks : Int,pendingLadders : Int) : Int = {
      //println(index + " " + pendingBricks + " " + pendingLadders)
      if (index == heights.length-1) {
        index
      }else {
        //can we move to next one
        if (heights(index+1) <= heights(index)) {
          itr(index+1,pendingBricks,pendingLadders)
        }else {
          //its older

          val heigtDiff = heights(index+1) - heights(index)
          if (pendingBricks < heigtDiff && pendingLadders == 0) {
            index //can't move beyond current
          }else {
            if (pendingBricks >= heigtDiff) {
              itr(index+1,pendingBricks-heigtDiff, pendingLadders)
            }else {
              if (pendingLadders > 0) {
                itr(index+1,pendingBricks,pendingLadders-1)
              }else {
                index
              }
            }

            //             var option2 = index


            //             math.max(option1,option2)
          }

        }

      }
    }

    itr(0,bricks,ladders)
  }
}