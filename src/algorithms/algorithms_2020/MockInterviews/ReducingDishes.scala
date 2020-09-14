import scala.collection.mutable

case class Index(val nIndex : Int,val negSelected : List[Int])
object Solution {
  def maxSatisfaction(satisfaction: Array[Int]): Int = {
    //visited in this order
    val negativesArr = new mutable.ArrayBuffer[(Int,Int)] //(value,index)
    val positiveArr = new mutable.ArrayBuffer[(Int,Int)]
    for (j <- 0 to satisfaction.length-1) {
      val num = satisfaction(j)
      if (num < 0) {
        negativesArr.append((j,num))
      }else {
        positiveArr.append((j,num))
      }
    }

    positiveArr.sortInPlace()(new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._2.compareTo(y._2)

      }
    })
    negativesArr.sortInPlace()(new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        x._2.compareTo(y._2)
      }
    })

    //println(positiveArr)

    val positiveSum = positiveArr.foldRight(0)((newVal,s) => s + newVal._2)
    var basicLikes = 0
    var st = 1
//    println(positiveArr)
//    println(basicLikes)
    for (j <- 0 to positiveArr.length-1) {
      basicLikes = basicLikes + st*positiveArr(j)._2
      st = st + 1
    }

    //basicLikes
    def positiveLikes(startTime : Int) : Int = {

      val diff = startTime-1
      val res = basicLikes + diff*positiveSum
      //println(startTime + " " + res)
      res
    }
    def itr(nIndex : Int,negativesSelected : List[Int]) : Int = {
      if (nIndex < 0) {
        var time = negativesSelected.length+1
        var likes = positiveLikes(negativesSelected.length+1)
//        for ((_,pValue) <- positiveArr) {
//          likes = likes + pValue*time
//          time = time + 1
//        }

        time = 1
        for (nIndex <- negativesSelected) {
          likes = likes + negativesArr(nIndex)._2*time
          time = time+1
        }

        likes
      }else {
        //select nIndex or don't
        val opt1 = itr(nIndex - 1, nIndex :: negativesSelected)
        val opt2 = itr(nIndex - 1, negativesSelected)
        //cache += ((index,math.max(opt1, opt2)))
        math.max(opt1, opt2)
      }

    }

    itr(negativesArr.length-1,List())
  }

  def main(args: Array[String]): Unit = {
    println(maxSatisfaction(Array(-1,-8,0,5,-9)))
  }
}