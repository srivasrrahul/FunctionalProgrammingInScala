object Solution {
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
    var pending = n
    for (j <- 0 to flowerbed.length-1 if pending > 0) {
       var leftOccupied = false
       if (j-1 >= 0) {
         if (flowerbed(j-1) == 1) {
           leftOccupied = true
         }
       }

       var rightOccupied = false
       if (j+1 < flowerbed.length) {
         if (flowerbed(j+1) == 1) {
           rightOccupied = true
         }
       }

       if (leftOccupied == false && rightOccupied == false) {
         flowerbed(j) = 1
         pending= pending-1
       }

    }

    pending == 0
//    def itr(pending : Int,bed : Array[Int]) : Boolean = {
//      if (pending == 0) {
//        true
//      }else {
//
//        var res = false
//        //pick any non empty spot
//        for (j <- 0 to bed.length-1 if res == false) {
//          if (bed(j) == 0) {
//            var leftOccupied = false
//            if (j-1 >= 0) {
//              if (bed(j-1) == 1) {
//                leftOccupied = true
//              }
//            }
//
//            var rightOccupied = false
//            if (j+1 < bed.length) {
//              if (bed(j+1) == 1) {
//                rightOccupied = true
//              }
//            }
//
//            if (leftOccupied == false && rightOccupied == false) {
//              val arr = bed.clone()
//              arr(j) = 1
//              if (itr(pending-1,arr)) {
//                res = true
//              }
//            }
//          }
//        }
//
//        res
//      }
//    }
//
//    itr(n,flowerbed)
  }
}