import scala.collection.mutable

case class Index(val bRow : Int,val eRow : Int,val bCol : Int,val eCol : Int,val c : Int)
object Solution {
  def ways(pizza: Array[String], k: Int): Int = {
    //row apple count
    //count apple count
    val rowAppleIndex = new mutable.HashMap[Int,mutable.TreeSet[Int]]()
    for (j <- 0 to pizza.length-1) {
      rowAppleIndex.getOrElseUpdate(j,new mutable.TreeSet[Int]())
      for (k <- 0 to pizza(0).length-1) {
        if (pizza(j)(k) == 'A') {
          val cols = rowAppleIndex.getOrElseUpdate(j,new mutable.TreeSet[Int]())
          cols.add(k)
        }
      }
    }
    def isApplePresent(bRow : Int,eRow : Int,bCol : Int,eCol : Int) : Boolean = {
      //      var appleFound = false
      //      for (j <- bRow to eRow) {
      //        for (k <- bCol to eCol) {
      //          if (pizza(j)(k) == 'A') {
      //            appleFound = true
      //          }
      //        }
      //      }
      //
      //      appleFound
      var appleFound = false
      for (j <- bRow to eRow if appleFound == false) {
        val cols = rowAppleIndex.get(j).get
        if (cols.range(bCol,eCol+1).size > 0) {
          appleFound = true
        }
      }

      appleFound
    }

    val cache = new mutable.HashMap[Index,Option[Int]]()
    def itr(bRow : Int,eRow : Int,bCol : Int,eCol : Int,c : Int) : Option[Int] = {
      //println(c)
      if (c == 1) {
        //One cut pending
        var count = 0
        for (j <- bRow+1 to eRow) {
          if (isApplePresent(bRow,j-1,bCol,eCol) && isApplePresent(j,eRow,bCol,eCol)) {
            count = count + 1
          }
        }

        for (j <- bCol+1 to eCol) {
          if (isApplePresent(bRow,eRow,bCol,j-1) && isApplePresent(bRow,eRow,j,eCol)) {
            count = count + 1
          }
        }

        if (count > 0) {
          Some(count)
        }else {
          None
        }
      }else {
        val index = new Index(bRow,eRow,bCol,eCol,c)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var totalCount = 0
          for (j <- bRow + 1 to eRow) {
            //Cut bRow,j
            if (isApplePresent(bRow, j - 1, bCol, eCol)) {
              val count = itr(j, eRow, bCol, eCol, c - 1)
              if (count.isDefined) {
                totalCount = (totalCount + count.get) % (Math.pow(10,9)+7).toInt
              }
            }
          }

          for (j <- bCol + 1 to eCol) {
            if (isApplePresent(bRow, eRow, bCol, j - 1)) {
              val count = itr(bRow, eRow, j, eCol, c - 1)
              if (count.isDefined) {
                totalCount = (totalCount + count.get) % (Math.pow(10,9)+7).toInt
              }
            }
          }

          if (totalCount > 0) {
            cache += ((index,Some(totalCount)))
            Some(totalCount)
          } else {
            cache += ((index,None))
            None
          }
        }

        //Some(totalCount)
      }
    }

    if (k == 1) {
      if (isApplePresent(0,pizza.length-1,0,pizza(0).length-1)) {
        1
      }else {
        0
      }
    }else {
      val res = itr(0, pizza.length - 1, 0, pizza(0).length - 1, k - 1)
      if (res.isDefined) {
        res.get
      } else {
        0
      }
    }
  }
}