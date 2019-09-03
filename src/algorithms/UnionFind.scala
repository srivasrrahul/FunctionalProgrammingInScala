
class UnionFind(n : Int) {

  val arr : Array[Int] = new Array[Int](n)
  val size : Array[Int] = new Array[Int](n)
  init()

  def init() : Unit = {

    for (i <- 0 to n-1) {
      arr(i) = i
      size(i) = 1
    }
  }

  def root(x : Int) : Int = {
    var y = x
    while (arr(y) != y) {
      y = arr(y)
    }

    y
  }
  def addConnection(a : Int, b : Int) : Unit = {
    val rootA = root(a)
    val rootB = root(b)

    println("AddConn : rootA = " + rootA + " rootB = " + rootB)
    if (rootA != rootB) {
      if (size(a) > size(b)) {
        arr(rootB) = rootA
        size(rootA) += size(rootB)
      }else {
        arr(rootA) = rootB
        size(rootB) += size(rootA)
      }
    }else {
      println("Already exists")
    }
  }

  def find(a : Int,b : Int) : Boolean = {
    val rootA = root(a)
    val rootB = root(b)
    println("rootA = " + rootA + " rootB = " + rootB)
    rootA == rootB
  }

  override def toString: String = {
    val str = arr.mkString(",") + "   " + size.mkString(",")
    str
  }
}

object QuickFind {
  def main(args: Array[String]): Unit = {
    val uf = new UnionFind(10)
    uf.addConnection(1,2)
    uf.addConnection(3,4)
//    uf.addConnection(5,6)
    uf.addConnection(1,6)
    uf.addConnection(3,2)
    println(uf.find(3,6))
    println(uf)
  }

}