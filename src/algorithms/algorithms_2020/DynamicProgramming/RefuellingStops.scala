object Solution {
  def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
    if (stations.isEmpty) {
      if (startFuel-target >= 0) {
        0
      }else {
        -1
      }
    }else {
      val cols = stations.length + 1
      val rows = stations.length + 1

      val matrix = Array.ofDim[Int](rows, cols)
      for (j <- 0 to rows-1) {
        for (k <- 0 to cols-1) {
          matrix(j)(k) = Int.MinValue
        }
      }

      for (k <- 0 to cols - 2) {
        val fuelNeeded = stations(k)(0)
        //println(fuelNeeded)
        if (startFuel < fuelNeeded) {
          matrix(0)(k) = Int.MinValue
        } else {
          matrix(0)(k) = (startFuel - fuelNeeded)
        }
      }

      val fuelNeeded = target
      if (fuelNeeded > startFuel) {
        matrix(0)(cols - 1) = Int.MinValue
      } else {
        matrix(0)(cols - 1) = (startFuel - fuelNeeded)
      }


      //println(matrix(0).mkString(","))
      if (matrix(0)(cols - 1) != Int.MinValue) {
        //println(matrix(0).mkString(","))
        0
      } else {
        var found = -1
        for (j <- 1 to rows - 1 if found == -1) {
          for (k <- j to cols - 1) {
            var maxFuelValue = Int.MinValue
            val currentStationDistance = if (k == cols - 1) target else stations(k)(0)
            //println("p " + (k-1) + " " + matrix(j - 1)(k-1))

            for (p <- k - 1 to 0 by -1 if matrix(j - 1)(p) != Int.MinValue) {
              val fuelPending = (matrix(j - 1)(p) + stations(p)(1)) - (currentStationDistance - stations(p)(0))
              // println("Here " + j + " " + k + " " + p + "   "  +
              //   (matrix(j-1)(p) + stations(p)(1)) + " " + currentStationDistance + " " + stations(p)(0) + " " + fuelPending)

              if (fuelPending >= 0 && fuelPending > maxFuelValue) {
                maxFuelValue = fuelPending
              }
            }


            matrix(j)(k) = maxFuelValue
          }

          //println(j + " " + matrix(j).mkString(","))
          if (matrix(j).last != Int.MinValue) {

            found = j
          }
        }

        // for (j <- 0 to rows - 1) {
        //   println(matrix(j).mkString(","))
        // }
        found

      }
    }


  }

  def main(args: Array[String]): Unit = {
    //    val stations = Array(Array(10,60),Array(20,30),Array(30,30),Array(60,40))
    //    println(minRefuelStops(100,10,stations))

    //val stations = Arr
  }
}