import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks._

trait EncodedString
case class BasicEncodedString(val string: String) extends EncodedString
case class ComplexEncodedString(val count : Int,val encodedStrings: List[EncodedString]) extends EncodedString

object Solution {
  def decodeString(s: String): String = {

    def parseEncodedString(index : Int) : (EncodedString,Int) = {
      var j = index
      var count = new StringBuilder

      while (s(j) >= '0' && s(j) <= '9') {
        count.append(s(j))
        j = j + 1
      }

      if (j == index) {
        //It's a plain string
        val stringBuilder = new StringBuilder
        while (j < s.length && false == (s(j) >= '0' && s(j) <= '9')) {
          stringBuilder.append(s(j))
          j = j + 1
        }

        //println("Returning plain string with index " + stringBuilder.toString() + " " + j)

        (new BasicEncodedString(stringBuilder.toString()),j-1)

      }else {

        val countVal = count.toString().toInt

        //j is at '['


        //extract till next ']'
        var openEncountered = 0

        val beginAfterSquare = j + 1

        var limit = -1

        breakable {
          while (true) {
            s(j) match {
              case '[' => {
                openEncountered = openEncountered + 1
              }
              case ']' => {
                openEncountered = openEncountered - 1
                if (openEncountered == 0) {
                  limit = j
                  break
                }
              }
              case _ => {

              }
            }

            j = j + 1
          }
        }


        val lst = new ListBuffer[EncodedString]
        var k = beginAfterSquare

        var accumulatedStr = new StringBuilder

        while (k <= limit - 1) {
          //println(s(k))
          s(k) match {
            case dig if dig >= '0' && dig <= '9' => {
              if (accumulatedStr.length() > 0) {
                lst.append(new BasicEncodedString(accumulatedStr.toString()))
                accumulatedStr.clear()
              }

              val (nextParsedEncodedString, parsedIndexEnd) = parseEncodedString(k)
              lst.append(nextParsedEncodedString)
              k = parsedIndexEnd + 1

            }
            case _ => {
              accumulatedStr.append(s(k))
              k = k + 1
            }
          }
        }

        if (accumulatedStr.length() > 0) {
          lst.append(new BasicEncodedString(accumulatedStr.toString()))
          accumulatedStr.clear()
        }

        (new ComplexEncodedString(countVal, lst.toList), limit)

      }
    }



    val lst = new ListBuffer[EncodedString]
    var j = 0
    while (j <= s.length-1) {
      val (encodedStr,parsedTillIndex) = parseEncodedString(j)
      j = parsedTillIndex+1

      lst.append(encodedStr)
    }

    //println(lst.toList)

    def encodedString(encodedStrings: List[EncodedString]) : String = {
      val stringBuilder = new StringBuilder
      encodedStrings.foreach(encodedStr => {
        encodedStr match {
          case BasicEncodedString(string: String) => {
            stringBuilder.append(string)
          }
          case ComplexEncodedString(count, localEncodedStrings: List[EncodedString]) => {
            val localString = encodedString(localEncodedStrings)
            for (j <- 0 to count-1) {
              stringBuilder.append(localString)
            }


          }
        }


      })

      stringBuilder.toString()
    }

    encodedString(lst.toList)
    //""


  }

  def main(args: Array[String]): Unit = {
    println(decodeString("2[abc]3[cd]ef"))
  }
}