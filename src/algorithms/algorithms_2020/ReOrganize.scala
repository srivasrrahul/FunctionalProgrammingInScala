import scala.collection.mutable
import util.control.Breaks._


object Solution {

  def reorganizeString(S: String): String = {
    val letterCount = new mutable.HashMap[Char,Int]()
    val countLetter = new mutable.TreeMap[Int,mutable.HashSet[Char]]()(Ordering[Int].reverse)

    for (ch <- S) {
      val oldCount = letterCount.getOrElseUpdate(ch,0)
      letterCount += ((ch,oldCount+1))
    }

    letterCount.foreachEntry((ch,count) => {
      val oldSet = countLetter.getOrElseUpdate(count,new mutable.HashSet[Char])
      oldSet.add(ch)
    })

    //println(letterCount)
    //println(countLetter)

    def getTopLetterWithMostCount(prevChar : Char) : Option[Char] = {
      //called with at least one element present
      //println("PrevChar passed " + prevChar)
      var result : Option[(Char,Int)] = None

      breakable {
        for (element <- countLetter) {
          if (element._1 > 0 && element._2.size > 0) {
            for (ch <- element._2) {
              //println("Ch is " + ch)
              if (ch != prevChar) {
                result = Some((ch,element._1))
                break()
              }
            }

          }

        }
      }

      result match {
        case None => None
        case Some((resCh,existingCount)) => {
          countLetter.get(existingCount).get.remove(resCh)
          countLetter.getOrElseUpdate(existingCount-1,new mutable.HashSet[Char])
          countLetter.get(existingCount-1).get.add(resCh)
          Some(resCh)
        }
      }

      result match {
        case None => {
          None
        }
        case Some((resCh,resCount)) => {
          //reduce count
          //println(resCh + " " + resCount)
          letterCount += ((resCh,resCount-1))
          countLetter.get(resCount).get.remove(resCh)
          countLetter.getOrElseUpdate(resCount-1,mutable.HashSet[Char]())
          countLetter.get(resCount-1).get.add(resCh)
          Some(resCh)

        }
      }
    }

    val stringBuilder = new StringBuilder
    var result = false
    var prevChar = '0'
    breakable {
      while (true) {
        if (stringBuilder.length() == S.length) {
          result = true
          break
        }

        val topLetter = getTopLetterWithMostCount(prevChar)
        //println(topLetter)
        //println(countLetter)
        //println(letterCount)
        topLetter match {
          case None => {
            result = false
            break
          }
          case Some(ch) => {
            stringBuilder.append(ch)
            prevChar = ch
          }
        }
      }
    }

    if (result == false) {
      ""
    }else {
      stringBuilder.toString()
    }

  }


  def main(args: Array[String]): Unit = {
    println(reorganizeString("rvhrlpiesrrryrbrrrrrxrkirranrrrrbdrrzgasoxrrr"))
    //println(reorganizeString("abcda"))
  }


}