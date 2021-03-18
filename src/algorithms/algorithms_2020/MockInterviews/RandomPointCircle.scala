import scala.util.Random

class Solution(_radius: Double, _x_center: Double, _y_center: Double) {

  def genPointWithinLimit() : Array[Double] = {
    var randomPointX = 0.0
    var randomPointY = 0.0

    var found = false
    while (found == false) {
      randomPointX = Random.between(_x_center-_radius,_x_center+_radius)
      randomPointY = Random.between(_y_center-_radius,_y_center+_radius)
      val d = scala.math.sqrt((randomPointX-_x_center)*(randomPointX-_x_center) + (randomPointY-_y_center)*(randomPointY-_y_center))
      if (d < _radius) {
        found = true
      }
    }
    Array(randomPointX,randomPointY)
  }
  def randPoint(): Array[Double] = {
   genPointWithinLimit()
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    val s  = new Solution(10.0,2.0,3.0)
    for (j <- 0 to 10)
      println(s.randPoint().mkString(","))
  }
}