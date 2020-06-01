object Solution {
  def plusOne(digits: Array[Int]): Array[Int] = {
    val (newLst,carry) = digits.foldRight((List[Int](),1))((newVal,accumulator) => {
      val (accumulatedLst,carry) = accumulator
      if (carry == 0) {
        ((newVal :: accumulatedLst),0)
      }else {
        val addedValue = carry + newVal
        if (addedValue <= 9) {
          ((addedValue :: accumulatedLst),0)
        }else {
          val currentVal = addedValue % 10
          val newCarry = addedValue / 10
          ((currentVal :: accumulatedLst),newCarry)
        }
      }
    })

    if (carry > 0) {
      (carry :: newLst).toArray
    }else {
      newLst.toArray
    }
  }
}