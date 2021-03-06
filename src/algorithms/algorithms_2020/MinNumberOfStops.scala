import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
    if (stations.length == 0) {
      if (startFuel >= target) {
        0
      }else {
        -1
      }
    }else {
      //val stationFuelIndex = new Array[List[(Int, Int)]](stations.length)
      val stationFuelIndex = new Array[mutable.HashMap[Int,Int]](stations.length)
      //Each index indicates car stops at index with set of fuel options and number of stops
      //empty indicates no such option exists
      //initialize
      val fuelPending = startFuel - stations(0)(0)
      if (fuelPending >= 0) {
        val map = new mutable.HashMap[Int, Int]()
        map += ((1,fuelPending))
        stationFuelIndex(0) = map
      } else {
        stationFuelIndex(0) = new mutable.HashMap[Int, Int]()
      }

      for (j <- 1 to stations.length - 1) {
        //val lst = new mutable.ArrayBuffer[(Int, Int)]
        val lst = new mutable.HashMap[Int,Int]() //hops petrol Available
        for (k <- j - 1 to 0 by -1) {
          val kthList = stationFuelIndex(k)
          for ((stopsAtK, fuelInTankAtStationK) <- kthList) {
            val pendingFuel = (fuelInTankAtStationK + stations(k)(1)) - (stations(j)(0) - stations(k)(0))
            if (pendingFuel >= 0) {
              val newKey = stopsAtK+1
              lst.get(newKey) match {
                case None => {
                  lst += ((newKey,pendingFuel))
                }
                case Some(oldPendingFuel) => {
                  //Select option with largest pending Fuel
                  if (pendingFuel > oldPendingFuel) {
                    lst += ((newKey,pendingFuel))
                  }
                }
              }
              //lst.append((stopsAtK + 1, pendingFuel))
            }
          }
        }

        //base case for first stop
        val pendingFuel = startFuel - stations(j)(0)
        if (pendingFuel >= 0) {
          lst.get(1) match {
            case None => lst += ((1, pendingFuel))
            case Some(oldPendingFuel) => {
              if (pendingFuel > oldPendingFuel) {
                lst += ((1, pendingFuel))
              }
            }
          }
        }


        //println(lst)

        stationFuelIndex(j) = lst
      }

      //println(stationFuelIndex.mkString("\n"))


      var minCount = Int.MaxValue
      for (j <- 0 to stations.length - 1) {
        for ((stopsAtJ, fuelInTankAtStationJ) <- stationFuelIndex(j)) {
          val fuelNeeded = (target - stations(j)(0))
          if ((fuelInTankAtStationJ + stations(j)(1)) >= fuelNeeded) {
            //distance possible
            if (stopsAtJ < minCount) {
              minCount = stopsAtJ
            }

          }
        }
      }

      //last case
      val pendingFuel = startFuel - target
      if (pendingFuel >= 0) {
        minCount = 0
      }

      if (minCount == Int.MaxValue) {
        -1
      }else {
        minCount
      }
    }
  }


  def main(args: Array[String]): Unit = {
    //val stations = Array(Array(10,100))
    val stations = Array(Array(10,60),Array(20,30),Array(30,30),Array(60,40))
    //val stations = Array(Array(5,100),Array(997,100),Array(998,100))
    //println(minRefuelStops(100,10,stations))
    println(minRefuelStops(100,50,Array(Array(25,25),Array(50,50))))

    //val arrStr = "[[1,186],[145,161],[183,43],[235,196],[255,169],[263,200],[353,161],[384,190],[474,44],[486,43],[567,48],[568,96],[592,36],[634,181],[645,167],[646,69],[690,52],[732,28],[800,42],[857,55],[922,63],[960,141],[973,13],[977,112],[997,162]]"
  }
}