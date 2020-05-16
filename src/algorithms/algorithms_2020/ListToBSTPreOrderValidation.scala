object Solution {
  def verifyPreorder(preorder: Array[Int]): Boolean = {
    val stack = new scala.collection.mutable.Stack[Int]
    val lstBuffer = new scala.collection.mutable.ListBuffer[Int]

    for (element <- preorder) {
      stack.isEmpty match {
        case true => {
          stack.push(element)
        }
        case false => {
          val top = stack.top
          if (element < top) {
            stack.push(element)
          }else {
            while (stack.isEmpty == false && element > stack.top ) {
              val topElement = stack.pop
              lstBuffer.append(topElement)
            }

            stack.push(element)
          }
        }
      }
    }


    //println(lstBuffer)
    //println(stack)
    while (stack.isEmpty == false) {
      val topElement = stack.pop
      lstBuffer.append(topElement)
    }

    //check if arr is sorted
    var sorted = true
    var lastValue : Option[Int] = None
    for (element <- lstBuffer if sorted == true) {
      lastValue match {
        case None => {
          lastValue = Some(element)
        }
        case Some(prevValue) => {
          if (element < prevValue) {
            sorted = false
          }else {
            lastValue = Some(element)
          }
        }
      }
    }

    sorted
  }
}