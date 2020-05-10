import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

case class BarRange(val low : Int,val high : Int)
object Solution {
  def calculatePotentiaWaterStored(heightArr : Array[Int],fromIndex : Int,toIndex : Int) : Int= {
    //println("Between index " + toIndex + " " + fromIndex)
    val minHeight = scala.math.min(heightArr(fromIndex),heightArr(toIndex))
    val potentialWater = minHeight * (fromIndex-toIndex-1)
    var waterStored = potentialWater
    for (j <- fromIndex-1 to toIndex+1 by -1) {
      //println("For j " + j + " wayer " + heightArr(j))
      waterStored = waterStored - heightArr(j)
    }

    //println("Between index " + toIndex + " " + fromIndex + " " + waterStored + " minheight " + minHeight + " potential " + potentialWater)
    if (waterStored < 0) {
      0
    }else {
      waterStored
    }

  }
  def trap(height: Array[Int]): Int = {
    var maxStored = 0
    val rangeValue = new mutable.HashMap[BarRange,Int]()
    val validRange = new mutable.ArrayBuffer[BarRange]()
    //no water stored in first
    for (j <- 1 to height.length-1) {
      //find max height before it and if space don't consider it

      var maxHeightBefore: Option[Int] = None
      breakable {
        for (k <- j - 1 to 0 by -1) {
          if (height(k) != 0) {
            maxHeightBefore match {
              case None => {
                maxHeightBefore = Some(k)
//                if (height(k) > height(j)) {
//                  break
//                }
              }
              case Some(foundHeightIndex) => {
                if (height(k) > height(foundHeightIndex)) {
                  maxHeightBefore = Some(k)
                }

//                if (height(k) > height(j)) {
//                  break
//                }
              }
            }

            if (height(k) >= height(j)) {
              break
            }
          }
        }
      }

      maxHeightBefore match {
        case Some(heightIndexFound) => {

          val waterStored = calculatePotentiaWaterStored(height, j, heightIndexFound)
          if (waterStored > 0) {
            val newRange = new BarRange(heightIndexFound, j)
            rangeValue += ((newRange, waterStored))
            validRange.append(newRange)
          }


        }
        case _ => {
          //No water addition
        }
      }





    }

    if (validRange.size > 0) {
      validRange.sortInPlace()(new Ordering[BarRange] {
        override def compare(x: BarRange, y: BarRange): Int = {
          x.low.compareTo(y.low)
        }
      })

      //println(validRange)

      val updatedRange = new ListBuffer[BarRange]
      updatedRange.addOne(validRange(0))


      //println(updatedRange)
      for (j <- 1 to validRange.length - 1) {
        val currentRange = validRange(j)
        if (currentRange.high >= updatedRange.last.high && currentRange.low <= updatedRange.last.low) {
          //current range is superse
          updatedRange.dropRightInPlace(1)
          updatedRange.append(currentRange)
        }else {
          if (updatedRange.last.high >= currentRange.high && updatedRange.last.low <= currentRange.low) {
            //valid range is superserv
            //dont' add
            //println("Here")
          }else {
            updatedRange.append(currentRange)
          }
        }

      }

      //println(updatedRange.mkString("\n"))
      updatedRange.foldLeft(0)((accumulator,newRange) => {
        accumulator + rangeValue.get(newRange).get
      })
    }else {
      0
    }


    //0

  }

  def main(args: Array[String]): Unit = {
    println(trap(Array(0,1,0,2,1,0,1,3,2,1,2,1)))
    println(trap(Array(2,0,2)))
    println(trap(Array(4,2,3)))
    //println(trap(Array(4,2,0,3,2,5)))
    println(trap(Array(5,4,1,2)))

    println(trap(Array(5,2,1,2,1,5)))
  }
}