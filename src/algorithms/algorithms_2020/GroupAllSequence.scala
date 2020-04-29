import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Solution {
  def diff(ch1 : Char, ch2: Char)  : Int=  {
    val d = ch2 - ch1
    d

  }
  def isSameShift(s1 : String,s2 : String) : Boolean = {
    if (s1.length == s2.length) {
      if (s1(0) > s2(0)) {
        isSameShift(s2,s1)
      }else {
        val firstDiff = diff(s1(0),s2(0))
        var retValue = true
        breakable {
          for (j <- 1 to s1.length - 1) {
            var nextShift = ((s1(j) + firstDiff))
            if (nextShift > 'z') {
              //println("Here")
              nextShift = 'a' + (nextShift - 'z') - 1
              //println(nextShift)
            }
            //println(nextShift + " " + s2(j))
            if (nextShift != s2(j)) {
              retValue = false
              break
            }
          }
        }

        retValue
      }
    }else {
      false
    }
  }




  def groupStrings(strings: Array[String]): List[List[String]] = {

    val groupArr = new mutable.ArrayBuffer[mutable.HashSet[Int]]
    val groupStringsToArrIndex = new mutable.HashMap[Int,Int]()

    for (j <- 0 to strings.length-1) {
      for (k <- j + 1 to strings.length-1) {
        if (isSameShift(strings(j),strings(k))) {
          if (groupStringsToArrIndex.contains(j) == false) {
            val currentIndex = groupArr.length

            groupStringsToArrIndex += ((j,currentIndex))
            groupStringsToArrIndex += ((k,currentIndex))

            groupArr.append(mutable.HashSet[Int](j,k))
          }else {
            val index = groupStringsToArrIndex.get(j).get
            groupArr(index).add(k)
            groupStringsToArrIndex += ((k,index))
          }
        }
      }
    }

    for (j <- 0 to strings.length-1) {
      if (groupStringsToArrIndex.contains(j) == false) {
        val currentIndex = groupArr.length
        groupStringsToArrIndex += ((j,currentIndex))
        groupArr.append(mutable.HashSet[Int](j))
      }
    }

    groupArr.foldRight(ListBuffer[List[String]]())((newSet,accumulated) => {
      val lstBuffer = new ListBuffer[String]
      for (index <- newSet) {
        lstBuffer.append(strings(index))
      }
      accumulated.append(lstBuffer.toList)
    }).toList
    //List(List(""))
  }

  def main(args: Array[String]): Unit = {
//    println(diff('a','z'))
//    println(isSameShift("abc","xyz"))

      //println(groupStrings(Array("abc", "bcd", "acef", "xyz", "az", "ba", "a", "z")))
//    println(groupStrings(Array("a", "a")))

    println(groupStrings(Array("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")))
  }

}