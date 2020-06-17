import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Solution {
  def asteroidCollision(asteroids: Array[Int]): Array[Int] = {
    var resArr = new ArrayBuffer[Int]()

    def merge(latestAsteroid : Int) : Unit = {
      if (resArr.size > 0) {
        val last = resArr.last
        if (last < 0 && latestAsteroid > 0) {
          //going in different direction
          resArr.append(latestAsteroid)
        }

        if (last > 0 && latestAsteroid > 0) {
          //both going in same direction
          resArr.append(latestAsteroid)
        }

        if (last < 0 && latestAsteroid < 0) {
          resArr.append(latestAsteroid)
        }

        if (last > 0 && latestAsteroid < 0) {
          //collision
          if (scala.math.abs(last) > scala.math.abs(latestAsteroid)) {
            //No need to append
          }

          if (scala.math.abs(latestAsteroid) > scala.math.abs(last)) {
            resArr.dropRightInPlace(1)
            resArr.append(latestAsteroid)
          }

          if (scala.math.abs(latestAsteroid) == scala.math.abs(last)) {
            resArr.dropRightInPlace(1)
          }
        }


      }else {
        resArr.append(latestAsteroid)
      }
    }

    var prev = asteroids.toArray

    var notAdded = false
    while (notAdded == false) {
      for (j <- 0 to prev.length - 1) {
        merge(prev(j))
      }

      //println("Merged " + resArr.mkString(","))
      if (prev.length == resArr.length) {
        notAdded = true
      }else {
        prev = resArr.toArray
        resArr.clear()
        //println(resArr + " : " +prev.mkString(","))
      }
    }

    prev
  }

  def main(args: Array[String]): Unit = {
    println(asteroidCollision(Array(-2, -1, 1, 2)).mkString(","))
  }
}