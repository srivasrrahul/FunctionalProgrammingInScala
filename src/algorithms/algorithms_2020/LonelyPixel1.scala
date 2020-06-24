import scala.collection.mutable

object Solution {
  def findLonelyPixel(picture: Array[Array[Char]]): Int = {
    //Not in the same row or column any black pixel
    if (picture.length == 0) {
      0
    }else {
      val maxRows = picture.length //empty handling
      val maxCols = picture(0).length //what happens if len is 0?
      val rowBlackMap = new mutable.HashMap[Int, Int]()
      val colBlackMap = new mutable.HashMap[Int, Int]()

      for (j <- 0 to maxRows - 1) {
        for (k <- 0 to maxCols - 1) {
          if (picture(j)(k) == 'B') {
            val rowPrevCount = rowBlackMap.getOrElseUpdate(j, 0)
            rowBlackMap += ((j, rowPrevCount + 1))

            val colPrevCount = colBlackMap.getOrElseUpdate(k, 0)
            colBlackMap += ((k, colPrevCount + 1))
          }
        }
      }

      var count = 0
      for (j <- 0 to maxRows - 1) {
        for (k <- 0 to maxCols - 1) {
          if (picture(j)(k) == 'B') {
            if (rowBlackMap.getOrElse(j, 0) == 1 && colBlackMap.getOrElse(k, 0) == 1) {
              count = count + 1
            }
          }

        }
      }

      count
    }
  }

  def main(args: Array[String]): Unit = {
    val picture = Array(
      Array('W', 'W', 'B'),
      Array('W', 'B', 'W'),
      Array('B', 'W', 'W')
    )

    println(findLonelyPixel(picture))
  }
}