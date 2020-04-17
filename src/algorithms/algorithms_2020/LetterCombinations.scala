import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def letterCombinations(digits: String): List[String] = {
    val digitMap = new mutable.HashMap[Char,List[Char]]()
    digitMap += (('2',List('a','b','c')))
    digitMap += (('3',List('d','e','f')))
    digitMap += (('4',List('g','h','i')))
    digitMap += (('5',List('j','k','l')))
    digitMap += (('6',List('m','n','o')))
    digitMap += (('7',List('p','q','r','s')))
    digitMap += (('8',List('t','u','v')))
    digitMap += (('9',List('w','x','y','z')))

    def itr(digitIndex : Int) : List[String] = {
      if (digitIndex == digits.length-1) {
        val lstBuffer = new ListBuffer[String]
        digitMap.get(digits(digitIndex)) match {
          case Some(chLst) => {
            chLst.foreach(ch => {
              lstBuffer.addOne(ch.toString)
            })
          }
          case None => {

          }
        }

        lstBuffer.toList
      }else {
        val nextLst = itr(digitIndex+1)
        val lstBuffer = new ListBuffer[String]
        digitMap.get(digits(digitIndex)) match {
          case Some(chLst) => {
            chLst.foreach(ch => {
              nextLst.foreach(nextStr => {
                lstBuffer.addOne(ch.toString + nextStr)
              })
            })
          }
          case None => {

          }
        }


        lstBuffer.toList
      }
    }

    if (digits.length == 0) {
      List[String]()
    }else {
      itr(0)
    }


  }

  def main(args: Array[String]): Unit = {
    println(letterCombinations("23"))
  }
}