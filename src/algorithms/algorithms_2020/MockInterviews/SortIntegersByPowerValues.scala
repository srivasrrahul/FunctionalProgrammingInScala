import scala.collection.mutable

object Solution {
  def getKth(lo: Int, hi: Int, k: Int): Int = {
    val cache = new mutable.HashMap[Int,Int]()
    def powerValue(j : Int) : Int = {
      if (j == 1) {
        0
      }else {
        if (cache.contains(j)) {
          cache.get(j).get
        }else {
          var retValue = 0
          if (j % 2 == 0) {
            retValue = 1+powerValue(j/2)
          }else {
            retValue = 1+powerValue(3*j+1)
          }

          cache += ((j,retValue))
          retValue
        }

      }
    }

    val arrBuffer = new mutable.ArrayBuffer[Int]
    for (x <- lo to hi) {
      arrBuffer.append(x)
    }

    arrBuffer.sortInPlace()(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = {

        val retValue = powerValue(x).compare(powerValue(y))
        if (y == 0) {
          x.compare(y)
        }else {
          retValue
        }
      }
    })

    arrBuffer(k-1)

  }
}