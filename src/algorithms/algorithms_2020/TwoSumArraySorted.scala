object Solution {
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    var i = 0
    var j = numbers.length-1
    var solution = new Array[Int](2)
    var found = false
    while (i < j && found == false) {
      //println(i + " " + j)
      if (numbers(i) + numbers(j) > target) {
        j = j-1
      }else {
        if (numbers(i) + numbers(j) < target) {
          i = i + 1
        }else {
          solution(0) = i+1
          solution(1) = j+1
          found = true
        }
      }
    }

    solution
  }
}