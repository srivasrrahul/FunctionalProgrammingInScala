import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def isPossible(nums: Array[Int]): Boolean = {
    val contents = new mutable.TreeMap[Int,Int]()
    for (num <- nums) {
      val defaultCount = contents.getOrElseUpdate(num,0)
      contents += ((num,defaultCount+1))
    }

    def getSize(map : scala.collection.immutable.SortedMap[Int,Int]) : Int = {
      var count = 0
      for ((k,v) <- map) {
        count = count+v
      }

      count
    }

    def isContuguos(map: scala.collection.immutable.SortedMap[Int,Int]) : Boolean = {
      //assume size is 3 as called only size is 3
      if (map.size < 3) {
        false
      }else {
        val first = map.head._1
        val second = map.tail.head._1
        val third = map.last._1
        if (first+1 == second && second+1 == third) {
          true
        }else {
          false
        }
      }
    }


    def checkIfAlreadyArranged(map : scala.collection.immutable.SortedMap[Int,Int]) : Boolean = {
      var valueCardinality = true
      for ((k,v) <- map) {
        if (v > 1) {
          valueCardinality = false
        }
      }

      if (valueCardinality) {
        var current = map.head._1
        var isPossible = true
        for ((k,v) <- map.tail) {
          //println(k + " " + current)
          if (k != current+1) {
            isPossible = false
          }else {
            current = k
          }
        }
        isPossible
      }else {
        false
      }
    }
    def removeFirstSortedN(map : scala.collection.immutable.SortedMap[Int,Int],n : Int) :
    (Boolean,scala.collection.immutable.SortedMap[Int,Int]) = {
      //n is greater than 3
      //val m1 = new ListBuffer[Int]
      val firstN = map.take(n)

      var current = firstN.head._1
      var isPossible = true
      for ((k,v) <- firstN.tail) {
        //println(k + " " + current)
        if (k != current+1) {
          isPossible = false
        }else {
          current = k
        }
      }

      if (isPossible) {
        val m1 = new mutable.TreeMap[Int,Int]()
        for ((k,v) <- map) {
          if (firstN.contains(k)) {
            if (v > 1) {
              m1 += ((k,v-1))
            }
          }else {
            m1 += ((k,v))
          }
        }

        (isPossible,m1.to(scala.collection.immutable.SortedMap))
      }else {
        (false,map)
      }

    }

    val cache = new mutable.HashMap[scala.collection.immutable.SortedMap[Int,Int],Boolean]
    def check(content : scala.collection.immutable.SortedMap[Int,Int]) : Boolean = {

      val count = getSize(content)
      //println("Check " + content + " " + count)
      if (count < 3) {
        false
      }else {
        if (count == 3) {
          isContuguos(content)
        }else {
          if (cache.contains(content)) {
            cache.get(content).get
          }else {
            if (checkIfAlreadyArranged(content)) {
              true
            }else {
              val keySize = contents.keys.size
              var possible = false
              //println("Check " + content + " " + keySize)
              for (p <- 3 to keySize if possible == false) {
                val (isPossible, remainingMap) = removeFirstSortedN(content, p)
                //println("Check " + content + " remove : " + p + " => " + isPossible + " " + remainingMap)
                if (isPossible) {
                  if (check(remainingMap)) {
                    possible = true
                  }
                }
              }

              //println("Check " + content + " " + possible)
              cache += ((content, possible))
              possible
            }
          }
        }
      }


    }

    //println(contents.to(scala.collection.immutable.SortedMap))
    check(contents.to(scala.collection.immutable.SortedMap))
  }

  def main(args: Array[String]): Unit = {
    println(isPossible(Array(4,5,6,7,7,8,8,9,10,11)))
  }
}