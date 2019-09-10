import util.control.Breaks._


object NumericalAlgorithmMain {
  def gcd(x : Int,y : Int) : Int = {
    println("x " + x + " y " + y )
    if (x < y) {
      gcd(y,x)
    }else {
      if (x == 0) {
        y
      }else {
        if (y == 0) {
          x
        }else {
          val r = x % y
          gcd(y,r)
        }
      }
    }
  }

  def gcd_loop(x : Int,y : Int) : Int = {
    def variable(x : Int,y : Int) : (Int,Int) = {
      if (x > y) {
        (x,y)
      }else {
        (y,x)
      }
    }

    var (a,b) = variable(x,y)
    var result = 1
    breakable {
      while (b > 1) {
        val r = a % b
        if (r == 0) {
          result = b
          break
        } else {
          a = b
          b = r
        }
      }
    }

    result
  }

  def randomize[T](arr : Array[T]): Unit = {
    def swap(i : Int, j : Int) : Unit = {
      val x = arr(i)
      arr(i) = arr(j)
      arr(j) = x
    }

    val r = new scala.util.Random
    for (i <- 0 to arr.length-1) {
      val next_int = r.between(i,arr.length)
      swap(i,next_int)
    }
  }

  def exponent(base : Long,exp : Long) : Long = {
    def exp_itr(n : Long) : Long = n match {
      case n if n == 1 => {
        base
      }
      case n if n % 2 == 0 => {
        val r = exp_itr(n/2)
        r*r
      }
      case n if n % 2 == 1 => {
        val r = exp_itr(n/2)
        r*r*base
      }
    }

    exp_itr(exp)
  }

  def find_prime_factors(n : Int) : List[Int] = {
    val lst = scala.collection.mutable.ListBuffer.empty[Int]
    val s : Int = Math.sqrt(n).toInt
    if (n % 2 == 0) {
      lst.addOne(2)
    }

    for (f <- 3 to s by 2) {
      if (f % 2 == 1 && n % f == 0) {
        var is_divisbile_by_previous = false
        breakable {
          for (x <- lst) {
            if (f % x == 0) {
              is_divisbile_by_previous = true
              break
            }
          }
        }

        if (is_divisbile_by_previous == false) {
          lst.addOne(f)
        }

      }
    }

    lst.toList
  }

  def main(args: Array[String]): Unit = {
    //println(gcd_loop(107,47))
    //val arr = Array(1,2,3)
    //randomize(arr)
    //println(arr.mkString(","))
    //println(exponent(2,45))
    println(find_prime_factors(961748941))
  }
}