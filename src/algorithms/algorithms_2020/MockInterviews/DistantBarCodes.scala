import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def rearrangeBarcodes(barcodes: Array[Int]): Array[Int] = {
    val pq = mutable.PriorityQueue.empty[(Int,Int)](new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._2.compare(y._2)
      }
    })

    val codeCount = new mutable.HashMap[Int,Int]()
    for (barCode <- barcodes) {
      val defCount = codeCount.getOrElseUpdate(barCode,0)
      codeCount += ((barCode,defCount+1))
    }

    for ((barCode,count) <- codeCount) {
      pq.addOne((barCode,count))
    }

    val retValue = new ArrayBuffer[Int]()

    var extracted = (-1,-1)

    while (pq.size != 0 || extracted._2 != -1) {
      if (extracted._2 == -1) {
        val (topKey,topCount) = pq.dequeue()
        retValue.append(topKey)
        if (topCount > 1) {
          extracted = (topKey,topCount-1)
        }
      }else {
        val (topKey,topCount) = pq.dequeue()
        retValue.append(topKey)
        pq.addOne(extracted)
        if (topCount > 1) {
          pq.addOne((topKey,topCount-1))
        }
        extracted = (-1,-1)
      }
    }

    retValue.toArray



  }
}