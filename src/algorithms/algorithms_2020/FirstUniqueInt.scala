import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class FirstUnique(_nums: Array[Int]) {


  val intCount = new mutable.HashMap[Int,Int]()
  val countInt = new mutable.TreeMap[Int,mutable.LinkedHashSet[Int]]()

  var time = 0

  for (num <- _nums) {
    val currentCount = intCount.getOrElseUpdate(num,0)
    intCount += ((num,currentCount+1))
  }

//  intCount.foreachEntry((value,count) => {
//    val currentLst = countInt.getOrElseUpdate(count,new ListBuffer[Int])
//    currentLst.append(value)
//  })

  for (num <- _nums) {
    val count = intCount.get(num).get
    val currentLst = countInt.getOrElseUpdate(count,new mutable.LinkedHashSet[Int])
    currentLst.add(num)

  }



  def showFirstUnique(): Int = {
    if (intCount.isEmpty == false) {
      val head = countInt.head
      if (head._1 == 1) {
        head._2.head
      }else {
        -1
      }
    }else {
      -1
    }
  }

  def add(num: Int) : Unit = {

    val prevCount = intCount.get(num)
    val currentCount = intCount.getOrElseUpdate(num,0)
    intCount += ((num,currentCount+1))

    val newCount = intCount.get(num).get

    prevCount match {
      case None => {
        //Inserting element for first time with this count

      }
      case Some(prevCountVal) => {
        countInt.get(prevCountVal).get.remove(num) //remove only a single instance
        if (countInt.get(prevCountVal).get.isEmpty) {
          countInt.remove(prevCountVal)
        }
      }
    }

    countInt.get(newCount) match {
      case Some(lst) => {
        lst.add(num)
      }
      case None => {
        val lst = new mutable.LinkedHashSet[Int]
        lst.add(num)

        countInt += ((newCount,lst))
      }
    }



  }

  def debug() : Unit = {
    println(intCount)
    println(countInt)
  }

}

object  Solution {
  def main(args: Array[String]): Unit = {
    //val arr = Array(2,3,5)
    var arr = Array(698,866,349,680,733,916,961,652,161,960,417,813,474,170,802,406,442,454,780,886,899,367,786,157,953,621,29,273,485,55,563,275,343,157,715,683,608,932,874,241,796,877,845,26,719,167,415,287,411,95,196,118,291,811,969,27,805,323,707,625,651,588,445,690,706,694,317,978,327,764,366,18,104,753,791,94,912,171,923,969,457,717,694,147,329,12,84,650,592,383,506,924,939,495,999,120,529,117,446,830,740,108,791,518,83,164,606,172,783,808)
    val firstUnique = new FirstUnique(arr)
    println(firstUnique.showFirstUnique())
//    firstUnique.debug()
    firstUnique.add(5)
    firstUnique.debug()
    println(firstUnique.showFirstUnique())
    firstUnique.add(2)
    println(firstUnique.showFirstUnique())
    firstUnique.add(3)
    println(firstUnique.showFirstUnique())
//    firstUnique.debug()



  }

}