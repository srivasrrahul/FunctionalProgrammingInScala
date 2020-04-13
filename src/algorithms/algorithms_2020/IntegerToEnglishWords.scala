import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DigiString {
  val digiString = new mutable.HashMap[Int,String]()
  for (j <- 0 to 9) {
    j match {
      case 0 => {
        digiString += ((0,""))
      }
      case 1 => {
        digiString += ((1,"One"))
      }
      case 2 => {
        digiString += ((2,"Two"))
      }
      case 3 => {
        digiString += ((3,"Three"))
      }
      case 4 => {
        digiString += ((4,"Four"))
      }
      case 5 => {
        digiString += ((5,"Five"))
      }
      case 6 => {
        digiString += ((6,"Six"))
      }
      case 7 => {
        digiString += ((7,"Seven"))
      }
      case 8 => {
        digiString += ((8,"Eight"))
      }
      case 9 => {
        digiString += ((9,"Nine"))
      }
    }
  }

  def getString(num : Int) : String = {
    digiString.get(num).get
  }
}

object DigiTenString {
  val digiString = new mutable.HashMap[Int,String]()
  for (j <- 0 to 99) {
    j match {
      case 0 => {
        digiString += ((0,""))
      }
      case 1 => {
        digiString += ((1,"One"))
      }
      case 2 => {
        digiString += ((2,"Two"))
      }
      case 3 => {
        digiString += ((3,"Three"))
      }
      case 4 => {
        digiString += ((4,"Four"))
      }
      case 5 => {
        digiString += ((5,"Five"))
      }
      case 6 => {
        digiString += ((6,"Six"))
      }
      case 7 => {
        digiString += ((7,"Seven"))
      }
      case 8 => {
        digiString += ((8,"Eight"))
      }
      case 9 => {
        digiString += ((9,"Nine"))
      }
      case 10 => {
        digiString += ((10,"Ten"))
      }
      case 11 => {
        digiString += ((11,"Eleven"))
      }
      case 12 => {
        digiString += ((12,"Twelve"))
      }
      case 13 => {
        digiString += ((13,"Thirteen"))
      }
      case 14 => {
        digiString += ((14,"Fourteen"))
      }
      case 15 => {
        digiString += ((15,"Fifteen"))
      }
      case 16 => {
        digiString += ((16,"Sixteen"))
      }
      case 17 => {
        digiString += ((17,"Seventeen"))
      }
      case 18 => {
        digiString += ((18,"Eighteen"))
      }
      case 19 => {
        digiString += ((19,"Nineteen"))
      }
      case twen if j >= 20 && j <= 29 => {
        val str = "Twenty " + DigiString.getString(j-20)
        digiString += ((j,str))
      }
      case thir if j >= 30 && j <= 39 => {
        val str = "Thirty " + DigiString.getString(j-30)
        digiString += ((j,str))
      }
      case thir if j >= 40 && j <= 49 => {
        val str = "Forty " + DigiString.getString(j-40)
        digiString += ((j,str))
      }
      case fif if j >= 50 && j <= 59 => {
        val str = "Fifty " + DigiString.getString(j-50)
        digiString += ((j,str))
      }
      case sixt if j >= 60 && j <= 69 => {
        val str = "Sixty " + DigiString.getString(j-60)
        digiString += ((j,str))
      }
      case sevent if j >= 70 && j <= 79 => {
        val str = "Seventy " + DigiString.getString(j-70)
        digiString += ((j,str))
      }
      case eigth if j >= 80 && j <= 89 => {
        val str = "Eighty " + DigiString.getString(j-80)
        digiString += ((j,str))
      }
      case nint if j >= 90 && j <= 99 => {
        val str = "Ninety " + DigiString.getString(j-90)
        digiString += ((j,str))
      }

    }
  }

  def getString(num : Int) : String = {
    digiString.get(num).get
  }
}

trait Parser {
  def parse(string : String) : String
}
object Basic extends Parser {
  override def parse(string: String): String = {
    val num = string.toInt
    if (num < 100) {
      if (num >= 10) {
        DigiTenString.getString(num)
      }else {
        DigiString.getString(num)
      }
    }else {
      val retValue = new StringBuilder
      retValue.append(DigiString.getString(num / 100) + " Hundred ")
      retValue.append(DigiTenString.getString(num % 100))
      retValue.toString()
    }
  }
}
object ThousandParser extends Parser {
  override def parse(string: String): String = {
    val prev = Basic.parse(string).trim
    if (prev.isEmpty) {
      ""
    }else {
      prev + " Thousand"
    }

  }
}

object MillionParser extends Parser {
  override def parse(string: String): String = {
    val prev = Basic.parse(string).trim
    if (prev.isEmpty) {
      ""
    }else {
      prev + " Million"
    }
  }
}

object BillionParser extends Parser {
  override def parse(string: String): String = {
    val prev = Basic.parse(string).trim
    if (prev.isEmpty) {
      ""
    }else {
      prev + " Billion"
    }
  }
}





object PositionToString {
  val positionToString = new mutable.HashMap[Int,Parser]()
  positionToString += ((0,Basic))
  positionToString += ((1,ThousandParser))
  positionToString += ((2,MillionParser))
  positionToString += ((3,BillionParser))
}



object Solution {

  def splitString(numStr : String) : List[String] = {
    val retVal = new ListBuffer[String]
    var count = 0
    val stringBuilder = new StringBuilder
    for (j <- numStr.length-1 to 0 by -1) {
      count += 1
      if (count > 3) {
        retVal.append(stringBuilder.reverse.toString())
        stringBuilder.clear()
        count = 1
      }

      stringBuilder.append(numStr.charAt(j))
    }

    if (stringBuilder.length() > 0) {
      retVal.addOne(stringBuilder.reverse.toString())
      stringBuilder.clear()
    }

    retVal.toList
  }
  def numberToWords(num: Int): String = {
    if (num == 0) {
      "Zero"
    }else {
      val numStr = String.valueOf(num)
      val lst = splitString(numStr)

      var count = 0
      val resultLst = new ListBuffer[String]
      lst.foreach(str => {
        resultLst.append(PositionToString.positionToString.get(count).get.parse(str))
        count = count + 1
      })

      //println(resultLst.reverse)
      val stringVal = resultLst.reverse.foldRight("") {
        (str, newVal) => {
          val trimmedNewVal = newVal.trim
          str + " " + trimmedNewVal
        }
      }

      stringVal.trim

    }

  }



  def main(args: Array[String]): Unit = {

    //println(digiTenString.digiString.mkString(","))
    //println(DigiTenString.getString(99))
    //println(splitString("5678"))
    println(numberToWords(0))
    println(numberToWords(11))
    println(numberToWords(1111))
    println(numberToWords(11111))
    println(numberToWords(111111))
    println(numberToWords(1111111))
    println(numberToWords(11111111))
    println(numberToWords(111111111))
    println(numberToWords(1111111111))


  }
}