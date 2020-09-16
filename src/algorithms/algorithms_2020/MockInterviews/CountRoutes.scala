import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val f : Int)
object Solution {
  def countRoutes(locations: Array[Int], start: Int, finish: Int, fuel: Int): Int = {
    val paths = new mutable.HashSet[List[Int]]()
    val cache = new mutable.HashMap[Index,List[List[Int]]]()
    def itr(j : Int,f : Int,pathsTillNow : List[List[Int]]) : List[List[Int]] = {
      if (j == finish && fuel == 0) {
        val lstBuffer = new ListBuffer[List[Int]]
        if (pathsTillNow.isEmpty) {
          List(List(j))
        }else {
          for (path <- pathsTillNow) {
            lstBuffer.append(j :: path)
          }

          lstBuffer.toList
        }
      }else {
        val index = new Index(j,f)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var pathFromHere = new ListBuffer[List[Int]]
          val updatedPathBuffer = new ListBuffer[List[Int]]
          if (pathsTillNow.isEmpty == false) {
            for (path <- pathsTillNow) {
              updatedPathBuffer.append(j :: path)
            }
          } else {
            updatedPathBuffer.append(List(j))
          }

          if (j == finish) {
            if (pathsTillNow.isEmpty) {
              pathFromHere.append(List(j))
            } else {
              for (path <- pathsTillNow) {
                pathFromHere.append(j :: path)
              }
            }
          }


          val updatedPathLst = updatedPathBuffer.toList
          for (k <- 0 to locations.length - 1) {
            if (j != k) {
              val fuelNeeded = math.abs(locations(j) - locations(k))
              if (f >= fuelNeeded) {
                val pathToK = itr(k, f - fuelNeeded, updatedPathLst)
                if (pathToK.isEmpty == false) {
                  for (path <- pathToK) {
                    pathFromHere.append(path)
                  }
                }
              }
            }
          }

          val retValue = pathFromHere.toList
          cache += ((index,retValue))
          retValue
        }


      }
    }

    val lst = itr(start,fuel,List())
    println(lst)
    lst.size
  }
}