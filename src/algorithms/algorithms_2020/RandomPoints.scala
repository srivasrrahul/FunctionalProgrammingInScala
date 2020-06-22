import scala.util.Random

class Solution(_radius: Double, _x_center: Double, _y_center: Double) {
  val randomX = new Random(System.currentTimeMillis())
  val randomY = new Random(System.currentTimeMillis())
  def randPoint(): Array[Double] = {
    val randX = randomX.between(0,2*3.14)
    val randY = randomY.between(0,2*3.14)

    val theta = Math.tanh(randY/randX)

    val x = _radius * Math.cos(theta)
    val y = _radius * Math.sin(theta)

    Array(x,y)
  }



}