import scala.collection.mutable

object Solution {

  def getManhattanDistance(worker : Array[Int],bike : Array[Int]) : Int = {
    scala.math.abs(worker(0)-bike(0)) + scala.math.abs(worker(1) - bike(1))

  }
  def assignBikes(workers: Array[Array[Int]], bikes: Array[Array[Int]]): Array[Int] = {
    //distance as key with a value again a treemap on worker index to bikeTreeMap phew
    val distanceMapping = new mutable.TreeMap[Int,mutable.TreeMap[Int,mutable.TreeSet[Int]]]()
    var bikeId = 0
    for (bike <- bikes) {
      var workerId = 0
      for (worker <- workers) {
        val distance = getManhattanDistance(worker,bike)
        distanceMapping.get(distance) match {
          case None => {
            val bikeSet = new mutable.TreeSet[Int]()
            bikeSet.add(bikeId)

            val workerMap = new mutable.TreeMap[Int,mutable.TreeSet[Int]]()
            workerMap += ((workerId,bikeSet))

            distanceMapping += ((distance,workerMap))
          }
          case Some(workerToBikeSet) => {
            workerToBikeSet.get(workerId) match {
              case Some(bikeSet) => {
                bikeSet.add(bikeId)
              }
              case None => {
                val bikeSet = new mutable.TreeSet[Int]()
                bikeSet.add(bikeId)

                workerToBikeSet += ((workerId,bikeSet))
              }
            }
          }
        }

        workerId = workerId + 1
      }

      bikeId = bikeId + 1
    }

    //println(distanceMapping.mkString("\n"))

    val assignedBikeToWorker = new mutable.HashMap[Int,Int]()
    val assignedBikes = new mutable.HashSet[Int]()

    for (distanceItr <- distanceMapping) {
      for (workerBikeSet <- distanceItr._2) {
        val workerId = workerBikeSet._1
        val bikeSet = workerBikeSet._2
        for (bikeId <- bikeSet) {
          assignedBikeToWorker.get(workerId) match {
            case None => {
              //println("Assinging " + workerId + " bike " + bikeId)
              if (assignedBikes.contains(bikeId) == false) {
                assignedBikeToWorker += ((workerId, bikeId))
                assignedBikes.add(bikeId)

              }
            }
            case _ => {
              //Won't assign
            }
          }
        }
      }
    }


    val workerBikeIndex = new Array[Int](workers.length)



    assignedBikeToWorker.foreachEntry((workerId,bikeId) => {
      workerBikeIndex(workerId) = bikeId
    })


    workerBikeIndex

  }

  def main(args: Array[String]): Unit = {
    val workers = Array(Array(0,0),Array(2,1))
    val bikes = Array(Array(1,2),Array(3,3))

    println(assignBikes(workers,bikes).mkString(","))
  }
}