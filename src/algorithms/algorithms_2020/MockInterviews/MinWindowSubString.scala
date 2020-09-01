import scala.collection.mutable

object Solution {
  def minWindow(s: String, t: String): String = {
    val cache = new mutable.HashMap[Char,mutable.TreeSet[Int]]()


    for (j <- 0 to s.length-1) {
      val ch = s(j)
      val set = cache.getOrElseUpdate(ch,new mutable.TreeSet[Int]())
      set.add(j)
    }

    val targetHashMap = new mutable.HashMap[Char,Int]()
    for (j <- 0 to t.length-1) {
      val ch = t(j)
      val defaultCount = targetHashMap.getOrElseUpdate(ch,0)
      targetHashMap += ((ch,defaultCount+1))
    }

    val target = targetHashMap.toMap
    def findAllChars(j : Int,k : Int) : Boolean = {
      val found = new mutable.HashMap[Char,Int]()
      for ((ch,set) <- cache) {
        val range = set.range(j,k+1)
        if (range.size > 0) {
          val defaultCount = found.getOrElseUpdate(ch,0)
          found += ((ch,defaultCount+range.size))
        }
      }

      var matchFound = true
      for ((tCh,tCount) <- target if matchFound) {
        if (found.contains(tCh) && found.get(tCh).get >= tCount) {

        }else {
          matchFound = false
        }
      }

      matchFound
    }




    var foundDiff : Option[(Int,Int)] = None
    for (j <- 0 to s.length-1) {
      for (k <- 0 to s.length-1) {

        if (findAllChars(j,k)) {
          val diff = k-j+1
          if (foundDiff.isDefined) {
            val (x1,x2) = foundDiff.get
            val count = x2-x1+1
            if (diff < count) {
              foundDiff = Some((j,k))
            }
          }else {
            foundDiff = Some((j,k))
          }
        }
      }
    }

    if (foundDiff.isDefined == false) {
      ""
    }else {
      val (x1,x2) = foundDiff.get
      s.substring(x1,x2+1)
    }
  }
}