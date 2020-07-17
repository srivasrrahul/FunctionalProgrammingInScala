object Solution {
  def compareVersion(version1: String, version2: String): Int = {

    val v1Arr = version1.split("\\.")
    val v2Arr = version2.split("\\.")

    var greaterValue = 0
    //println(v1Arr.mkString(","))
    var invalidInput = false
    for (j <- 0 to math.min(v1Arr.length-1,v2Arr.length-1) if greaterValue == 0 && invalidInput == false) {
      if (v1Arr(j) == "" || (v1Arr(j) forall Character.isDigit) == false || v2Arr(j) == "" || (v2Arr(j) forall Character.isDigit) == false) {
        invalidInput = true
      }else {
        val v1 = v1Arr(j).toInt
        val v2 = v2Arr(j).toInt
        v1.compareTo(v2) match {
          case 0 => {

          }
          case _ => {
            greaterValue = v1.compareTo(v2)
          }
        }
      }
    }

    if (invalidInput == false) {
      var j = math.min(v1Arr.length-1,v2Arr.length-1)+1

      if (greaterValue == 0) {
        if (j <= v1Arr.length-1) {
          //println("here " + v1Arr(j))
          var found = false
          while (j <= v1Arr.length-1 && found == false) {
            if (v1Arr(j).toInt == 0) {
              j = j + 1
            }else {
              greaterValue = 1
              found = true
            }
          }
        }else {
          if (j <= v2Arr.length-1) {
            var found = false
            while (j <= v2Arr.length - 1 && found == false) {
              if (v2Arr(j).toInt == 0) {
                j = j + 1
              } else {
                greaterValue = -1
                found = true
              }
            }
          }
        }
      }
    }
    //println(greaterValue)


    greaterValue
  }

  def main(args: Array[String]): Unit = {
    println(compareVersion("1.1",""))
  }
}