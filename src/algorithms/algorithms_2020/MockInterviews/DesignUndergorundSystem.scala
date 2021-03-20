import scala.collection.mutable

case class StationToFrom(val start : String,val end : String)

class UndergroundSystem() {

  val checkInchCheckOut = new mutable.HashMap[Int,(String,Int)]()
  val stationToFromAverage = new mutable.HashMap[StationToFrom,(Int,Double)]()

  def checkIn(id: Int, stationName: String, t: Int) : Unit =  {
    checkInchCheckOut += ((id,(stationName,t)))
  }

  def checkOut(id: Int, stationName: String, t: Int) : Unit = {
    val (stationCheckedIn,checkedInTime) = checkInchCheckOut.get(id).get
    val timeTaken = t-checkedInTime

    val pair = new StationToFrom(stationCheckedIn,stationName)
    val (oldCount,oldAverage) = stationToFromAverage.getOrElseUpdate(pair,(0,0.0))

    stationToFromAverage += ((pair,(oldCount+1,(oldAverage*oldCount + timeTaken)/(oldCount+1))))
    checkInchCheckOut.remove(id)

  }

  def getAverageTime(startStation: String, endStation: String): Double = {
    val pair = new StationToFrom(startStation,endStation)
    stationToFromAverage.get(pair).get._2
  }

}