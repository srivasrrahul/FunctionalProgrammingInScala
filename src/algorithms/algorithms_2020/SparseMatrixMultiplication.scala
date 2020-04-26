import scala.collection.mutable


object Solution {

  def multiply(A: Array[Array[Int]], B: Array[Array[Int]]) : Array[Array[Int]] = {
    val sparseValueA = new mutable.TreeMap[Int,Set[Int]]()

    for (j <- 0 to A.length-1) {
      val zeroIndexes = new mutable.HashSet[Int]()
      for (k <- 0 to A(0).length-1) {
        if (A(j)(k) == 0) {
          zeroIndexes.add(k)
        }
      }

      sparseValueA += ((j,zeroIndexes.toSet))
    }

    val sparseValueB = new mutable.TreeMap[Int,Set[Int]]()
    for (j <- 0 to B(0).length-1) {
      val zeroIndexes = new mutable.HashSet[Int]()
      for (k <- 0 to B.length-1) {
        if (B(k)(j) == 0) {
          zeroIndexes.add(k)
        }
      }

      sparseValueB += ((j,zeroIndexes.toSet))
    }

    val result = Array.ofDim[Int](A.length,B(0).length)

    val totalPotentialSet = new mutable.HashSet[Int]()
    for (j <- 0 to A(0).length-1) {
      totalPotentialSet.add(j)
    }

    val totalSet = totalPotentialSet.toSet

    var i = -1
    var j = -1

    //println(sparseValueA.mkString("\n"))

    //println(" ")
    //println(sparseValueB.mkString("\n"))
    for (sparseA <- sparseValueA) {
      i = i + 1
      j = 0
      for (sparseB <- sparseValueB) {
        val resultZeroIndex = sparseA._2.union(sparseB._2)
        val totalLength = A(0).length
        val netMultiplactiveIndex = totalSet.diff(resultZeroIndex)
        var netValue = 0

        //println("For i j " + i + " " + j + " " + netMultiplactiveIndex.mkString(",") + " " + (A(i)(j)*B(j)(i)))
        for (validIndex <- netMultiplactiveIndex) {
          netValue = netValue + A(i)(validIndex)*B(validIndex)(j)
        }

        result(i)(j) = netValue

        j = j + 1
      }
    }

    result

  }

  def main(args: Array[String]): Unit = {
    val a  = Array(Array(1,0,0),Array(-1,0,3))
    val b = Array(Array(7,0,0),Array(0,0,0),Array(0,0,1))

    val c = multiply(a,b)

    for (cdash <- c) {
      println(cdash.mkString(","))
    }

  }
}