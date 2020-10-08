import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def rankTeams(votes: Array[String]): String = {
    val rows = votes.length
    val cols = votes(0).length
    def rank(k : Int,set : Set[Char]) : List[Char] = {
      //println("Inside rank " + k + " " + set.toSet + " " + cols)
      if (k == cols-1) {
        val counter = new mutable.HashMap[Char,Int]()
        for (ch <- set) {
          counter += ((ch,0))
        }
        for (j <- 0 to rows-1) {
          val ch = votes(j)(k)
          if (set.contains(ch)) {
            val defCount = counter.getOrElseUpdate(votes(j)(k),0)
            counter += ((ch,defCount+1))
          }
        }

        val treeMap = new mutable.TreeMap[Int,mutable.TreeSet[Char]]()(Ordering[Int].reverse)
        for ((ch,count) <- counter) {
          val defSet = treeMap.getOrElseUpdate(count,new mutable.TreeSet[Char]())
          defSet.add(ch)
        }

        val retValue = new ListBuffer[Char]
        for ((_,treeSet) <- treeMap) {
          retValue.appendAll(treeSet)
        }


        retValue.toList
      }else {
        val counter = new mutable.HashMap[Char,Int]()
        for (ch <- set) {
          counter += ((ch,0))
        }

        for (j <- 0 to rows-1) {
          val ch = votes(j)(k)
          if (set.contains(ch)) {
            val defCount = counter.getOrElseUpdate(votes(j)(k),0)
            counter += ((ch,defCount+1))
          }
        }

        //println(counter)
        val treeMap = new mutable.TreeMap[Int,mutable.TreeSet[Char]]()(Ordering[Int].reverse)
        for ((ch,count) <- counter) {
          val defSet = treeMap.getOrElseUpdate(count,new mutable.TreeSet[Char]())
          defSet.add(ch)
        }

        val retValue = new ListBuffer[Char]
        for ((_,treeSet) <- treeMap) {
          if (treeSet.size > 1) {
            val sortedLst = rank(k+1,treeSet.toSet)
            retValue.appendAll(sortedLst)
            retValue.appendAll(treeSet.toSet.diff(sortedLst.toSet).toList)
          }else {
            retValue.appendAll(treeSet)
          }
        }


        retValue.toList
      }
    }

    var set = new mutable.HashSet[Char]()
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        set.add(votes(j)(k))
      }
    }

    val count = set.size
    //println(count)
    //rank(0,set.toSet).mkString("")
    val retValue = new StringBuilder
    for (k <- 0 to cols-1 if retValue.size < count) {

      val lst = rank(k,set.toSet)
      //println(lst)
      retValue.append(lst.mkString(""))
      set = set.diff(lst.toSet)

      //println(set)
    }

    retValue.toString()
  }

  def main(args: Array[String]): Unit = {

  }
}