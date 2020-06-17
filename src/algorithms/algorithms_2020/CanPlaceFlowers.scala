import scala.collection.mutable

object Solution {
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
    if (flowerbed.length == 1) {
      if (n == 0) {
        true
      }else {
        flowerbed.head == 0
      }
    }else {
      var pending = n
      for (j <- 0 to flowerbed.length - 1 if pending > 0) {
        if (flowerbed(j) == 0) {
          if (j == 0) {
            if (flowerbed(1) == 0) {
              flowerbed(0) = 1
              pending = pending - 1
            }
          }else {
            if (j == flowerbed.length - 1) {
              if (flowerbed(flowerbed.length-2) == 0) {
                flowerbed(flowerbed.length - 1) = 1
                pending = pending - 1
              }
            } else {
              if (flowerbed(j - 1) == 0 && flowerbed(j + 1) == 0) {
                //println(j)
                pending = pending - 1
                flowerbed(j) = 1
              }
            }
          }
        }
      }



      //println(pending)
      pending == 0
    }

  }

  def main(args: Array[String]): Unit = {
    println(canPlaceFlowers(Array(1,0,1,0,1,0,1),1))
  }
}