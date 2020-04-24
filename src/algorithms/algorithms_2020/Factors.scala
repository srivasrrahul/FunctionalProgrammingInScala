import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._



case class Factor(val a : Int,val b : Int)
object Solution {


  def getFactors(n: Int): List[List[Int]]  = {

    val factorTwo = new mutable.HashSet[Factor]()

    breakable {
      for (j <- 2 to n / 2) {
        if (n % j == 0) {
          val a = j
          val b = n / j
          val factor = new Factor(scala.math.min(a,b),scala.math.max(a,b))
          if (factorTwo.contains(factor )) {
            break()
          }else {
            factorTwo.add(factor)
          }
        }
      }
    }

    val retVal = new mutable.HashSet[List[Int]]
    for (factor <- factorTwo) {
      val factors = getFactors(factor.b)
      if (factors.isEmpty == false) {
        for (subFactor <- factors) {
          val newFactor = (List(factor.a) ++ subFactor).sortWith(_ < _)
          retVal.add(newFactor)
        }
      }
    }

    for (factor <- factorTwo) {
      retVal.add(List(factor.a,factor.b))
    }


    retVal.toList

  }

  def main(args: Array[String]): Unit = {
    println(getFactors(1))
  }
}