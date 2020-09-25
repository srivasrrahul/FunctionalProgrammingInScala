import scala.collection.mutable

case class Index(val bX : Int,val eX : Int,val bY : Int,val eY : Int)
object Solution {
  def tilingRectangle(m: Int, n: Int): Int = {
    val cache = new mutable.HashMap[Index,Int]()
    def itr(bX : Int,eX : Int,bY : Int,eY : Int) : Int = {
      //println(bX + "  " + eX + " " + bY + " " + eY)
      if (bX >= eX || bY >= eY) {
        0
      }else {
        if ((eX-bX == 1) || (eY-bY == 1)) {
          (eX-bX)*(eY-bY)
        }else {
          if (eX-bX == eY-bY) {
            1
          }else {
            val index = new Index(bX, eX, bY, eY)
            if (cache.contains(index)) {
              cache.get(index).get
            } else {
              var minTile = Int.MaxValue
              //reduce by left one
              for (p <- bX + 1 to eX - 1) {
                //split at p
                val opt = itr(bX, p, bY, eY) + itr(p, eX, bY, eY)
                if (opt < minTile) {
                  minTile = opt
                }
              }

              for (p <- bY + 1 to eY - 1) {
                val opt = itr(bX, eX, bY, p) + itr(bX, eX, p, eY)
                if (opt < minTile) {
                  minTile = opt
                }
              }

              cache += ((index,minTile))
              minTile
            }
          }
        }
      }
    }
    itr(0,m,0,n)
  }

  def main(args: Array[String]): Unit = {
    println(tilingRectangle(2,3))
  }
}