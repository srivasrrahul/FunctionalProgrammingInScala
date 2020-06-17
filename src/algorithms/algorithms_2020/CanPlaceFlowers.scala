import scala.collection.mutable

object Solution {
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
    def canPlantFlower(index : Int,addedSet : Set[Int]) : Boolean = {
      //specical for 0 an last
      val N = flowerbed.length-1
      index match {
        case 0 => {
          if (flowerbed(1) == 0 && addedSet.contains(1) == false) {
            true
          }else {
            false
          }
        }
        case N => {
          if (flowerbed(N-1) == 0 && addedSet.contains(N-1) == false) {
            true
          }else {
            false
          }
        }
        case _ => {
          (flowerbed(index-1) == 0 && flowerbed(index+1) == 0 && addedSet.contains(index-1) == false && addedSet.contains(index+1) == false)
        }
      }
    }
    def itr(pendingFlowers : Int, addedSet : Set[Int],pendingPlaces : Set[Int]) : Boolean = {
      pendingFlowers match {
        case 0 => true
        case _ => {
          var retValue = false
          for (pendingPlace <- pendingPlaces if retValue == false) {
            if (canPlantFlower(pendingPlace,addedSet)) {
              if (itr(pendingFlowers-1,addedSet.+(pendingPlace),pendingPlaces.-(pendingPlace))) {
                retValue = true
              }
            }
          }

          retValue
        }
      }
    }

    val pendingSet = new mutable.HashSet[Int]()
    for (j <- 0 to flowerbed.length-1) {
      if (flowerbed(j) == 0) {
        pendingSet.add(j)
      }
    }

    if (flowerbed.length==1) {
      if (n == 1) {
        flowerbed(0) == 0
      }else {
        if (n == 0) {
          true
        }else {
          false
        }
      }
    }else {
      itr(n, Set(), pendingSet.toSet)
    }
  }

  def main(args: Array[String]): Unit = {
    println(canPlaceFlowers(Array(1,0,0,0,1),2))
  }
}