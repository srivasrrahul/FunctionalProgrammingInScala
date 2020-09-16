import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val f : Int)
object Solution {
  def countRoutes(locations: Array[Int], start: Int, finish: Int, fuel: Int): Int = {
    val paths = new mutable.HashSet[List[Int]]()
    val cache = new mutable.HashMap[Index,Long]()
    def itr(j : Int,f : Int,pathsTillNow : Long) : Long = {
      if (j == finish && fuel == 0) {
        val lstBuffer = new ListBuffer[List[Int]]
        if (pathsTillNow == 0) {
          1
        }else {
          pathsTillNow
        }

      }else {
        val index = new Index(j,f)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          //          var pathFromHere = new ListBuffer[List[Int]]
          //          val updatedPathBuffer = new ListBuffer[List[Int]]
          //          if (pathsTillNow.isEmpty == false) {
          //            for (path <- pathsTillNow) {
          //              updatedPathBuffer.append(j :: path)
          //            }
          //          } else {
          //            updatedPathBuffer.append(List(j))
          //          }

          var pathFromHere : Long = 0
          var updatedPathBuffer : Long = 0
          if (pathsTillNow == 0) {
            updatedPathBuffer = 1
          }else {
            updatedPathBuffer = pathsTillNow
          }

          if (j == finish) {
            if (pathsTillNow == 0) {
              pathFromHere = 1
            } else {
              //              for (path <- pathsTillNow) {
              //                pathFromHere.append(j :: path)
              //              }
              pathFromHere = pathFromHere + pathsTillNow
            }
          }


          //val updatedPathLst = updatedPathBuffer.toList
          for (k <- 0 to locations.length - 1) {
            if (j != k) {
              val fuelNeeded = math.abs(locations(j) - locations(k))
              if (f >= fuelNeeded) {
                val pathToK = itr(k, f - fuelNeeded, updatedPathBuffer)
                if (pathToK > 0) {
                  pathFromHere = pathFromHere + pathToK
                }
              }
            }
          }

          //val retValue = pathFromHere.toList
          pathFromHere = pathFromHere % (Math.pow(10,9)+7).toLong
          cache += ((index,pathFromHere))
          pathFromHere
        }


      }
    }

    val count = itr(start,fuel,0)
    val intVal = count % (Math.pow(10,9)+7)
    intVal.toInt
  }
}