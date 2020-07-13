import scala.collection.mutable

object Solution {
  def numTeams(r : Array[Int]): Int = {

    def internal(rating: Array[Int]): Int = {
      val smallerThan = new Array[Int](rating.length)
      for (j <- 0 to rating.length - 1) {
        var count = 0
        for (k <- j - 1 to 0 by -1) {
          if (rating(k) < rating(j)) {
            count = count + 1
          }
        }

        smallerThan(j) = count
      }

      var countForward = 0
      for (j <- 0 to rating.length - 1) {
        for (k <- j - 1 to 0 by -1) {
          if (rating(k) < rating(j)) {
            val smallerThanK = smallerThan(k)
            countForward = countForward + smallerThanK
          }

        }
      }

      countForward
    }

    internal(r) + internal(r.reverse)

  }


  def main(args: Array[String]): Unit = {
    println(numTeams(Array(2,5,3,4,1)))
  }
}