import scala.collection.mutable.ArrayBuffer

object Solution {
  def diagonalSort(grid: Array[Array[Int]]): Array[Array[Int]] = {
    val rows = grid.length
    val cols = grid(0).length

    var j = rows-1
    var k = 0

    while (j >= 0) {
      val arrBuffer = new ArrayBuffer[Int]()
      var jj = j
      var kk = 0
      while (jj < rows && kk < cols) {
        arrBuffer.append(grid(jj)(kk))
        jj = jj + 1
        kk = kk + 1
      }

      //println(arrBuffer.mkString(","))
      arrBuffer.sortInPlace()
      jj = j
      kk = 0
      var index = 0
      while (jj < rows && kk < cols) {
        grid(jj)(kk) = arrBuffer(index)
        jj = jj + 1
        kk = kk + 1
        index = index+1
      }

      j = j -1
    }

    k = 0
    j = 0
    while (k < cols) {
      var jj = 0
      var kk = k

      val arrayBuffer = new ArrayBuffer[Int]()
      while (jj < rows && kk < cols) {
        arrayBuffer.append(grid(jj)(kk))
        jj = jj + 1
        kk = kk + 1
      }

      //println(arrayBuffer.mkString(","))
      arrayBuffer.sortInPlace()


      jj = 0
      kk = k
      var index = 0
      while (jj < rows && kk < cols) {
        grid(jj)(kk) = arrayBuffer(index)
        jj = jj + 1
        kk = kk + 1
        index = index+1
      }

      k = k + 1
    }

    grid
  }
}