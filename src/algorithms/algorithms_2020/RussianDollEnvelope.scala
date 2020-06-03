object Solution {
  def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {
    if (envelopes.length == 0) {
      0
    }else {
      envelopes.sortInPlace()(new Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val retValue = x(0).compareTo(y(0))
          if (retValue == 0) {
            x(0).compareTo(y(0))
          }else {
            retValue
          }

        }
      })

//      for (envelope <- envelopes) {
//        println(envelope.mkString(","))
//      }

      val maxEnvelopeCount = new Array[Int](envelopes.length)
      maxEnvelopeCount(0) = 1

      for (j <- 1 to envelopes.length - 1) {
        var maxCount = 1 //only self
        for (k <- j-1 to 0 by -1) {

          if (envelopes(k)(0) < envelopes(j)(0) && envelopes(k)(1) < envelopes(j)(1)) {
            val currentCount = 1 + maxEnvelopeCount(k)
            if (currentCount > maxCount) {
              maxCount = currentCount
            }
          }

        }

        maxEnvelopeCount(j) = maxCount
      }

      maxEnvelopeCount.max
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(5,4),Array(6,4),Array(6,7),Array(2,3))
    println(maxEnvelopes(arr))
  }
}