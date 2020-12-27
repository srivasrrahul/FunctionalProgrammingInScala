import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Solution {
  def minJumps(arr: Array[Int]): Int = {
    val valIndex = new mutable.HashMap[Int,mutable.HashSet[Int]]()
    for (j <- 0 to arr.length-1) {
      val defSet = valIndex.getOrElseUpdate(arr(j),new mutable.HashSet[Int]())
      defSet.add(j)
    }
    val countCache = new mutable.HashMap[Int,Option[Int]]()
    def itr(j : Int,visited : Set[Int]) : Option[Int] = {
      if (j == arr.length-1) {
        Some(0)
      }else {
        if (visited.contains(j)) {
          None
        }else {
          if (countCache.contains(j)) {
            countCache.get(j).get
          }else {
            val options = new ArrayBuffer[Int]()
            if (j + 1 < arr.length) {
              val res = itr(j + 1, visited.+(j))
              if (res.isDefined) {
                options.append(1 + res.get)
              }
            }

            if (j - 1 >= 0) {
              val res = itr(j - 1, visited.+(j))
              if (res.isDefined) {
                options.append(1 + res.get)
              }
            }

            for (k <- valIndex.get(arr(j)).get) {
              if (j != k && arr(j) == arr(k)) {
                val res = itr(k, visited.+(j))
                if (res.isDefined) {
                  options.append(1 + res.get)
                }
              }
            }


            if (options.isEmpty == false) {
              countCache += ((j,Some(options.min)))
              Some(options.min)
            } else {
              countCache += ((j,None))
              None
            }

          }
        }
      }
    }

    itr(0,Set()).get
  }
}