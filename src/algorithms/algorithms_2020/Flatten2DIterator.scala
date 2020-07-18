class Vector2D(v: Array[Array[Int]]) {
  var currentMajor = 0
  var currentMinor = 0
  def next(): Int = {
    while (currentMinor >= v(currentMajor).length) {
      currentMajor = currentMajor+1
      currentMinor = 0
    }
    val retValue = v(currentMajor)(currentMinor)
    currentMinor = currentMinor +1
    while (currentMajor < v.length && currentMinor >= v(currentMajor).length) {
      currentMajor = currentMajor+1
      currentMinor = 0
    }
    retValue
  }

  def hasNext(): Boolean = {
    //println("has next " + currentMajor + " " + currentMinor)
    while (currentMajor < v.length && v(currentMajor).length == currentMinor) {
      currentMajor = currentMajor + 1
      currentMinor = 0
    }
    if (currentMajor == v.length) {
      false
    }else {

      if (currentMajor == v.length-1 && v(currentMajor).length == currentMinor) {
        println(currentMajor + " " + currentMinor)
        false
      }else {
        true
      }

    }
  }

}