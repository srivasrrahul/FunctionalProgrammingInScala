sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A] (head : A,tail : MyList[A]) extends MyList[A]

object MyList {
  def sum(ints : MyList[Int]) : Int = ints match {
    case MyNil => 0
    case MyCons(x,xs) => x + sum(xs)
  }

  def length[A](lst : MyList[A]) : Int = lst match {
    case MyNil => 0
    case MyCons(x,xs) => 1 + length(xs)
  }

  def tail[A](lst : MyList[A]) : MyList[A] = lst match {
    case MyNil => MyNil
    case MyCons(x,xs) => xs
  }

  def setHead[A](x : A,lst : MyList[A]) : MyList[A] = lst match {
    case MyNil => MyNil
    case MyCons(_,xs) => MyCons(x,xs)
  }

  def drop[A](lst : MyList[A], n: Int) : MyList[A] = lst match {
    case MyNil => MyNil
    case MyCons(_,xs) => {
      if (n==0) {
        xs
      }else {
        drop(xs,n-1)
      }
    }
  }

  def dropWhile[A](lst : MyList[A],f : (A => Boolean)) : MyList[A] = lst match {
    case MyNil => MyNil
    case MyCons(x,xs) => {
      if (f(x) == true) {
        dropWhile(xs,f)
      }else {
        lst
      }
    }
  }

  def curry[A,B,C](f : (A,B) => C) : A => (B => C) = {
    def h(a : A) : (B => C) = {
      def g(b : B) : C = {
        f(a,b)
      }
      g
    }
    h
  }

  def uncurry[A,B,C](f : A => B => C) : (A,B) => C = {
    def h(a : A,b : B) : C = {
      val f1  = f(a)
      f1(b)
    }

    h
  }

  def foldRight[A,B](as : MyList[A],z : B)(f : (A,B) => B) : B = {
    as match {
      case MyNil => z
      case MyCons(x,xs)=> f(x,foldRight(xs,z)(f))
    }
  }

  def sum1(lst : MyList[Int]) : Int = {
    foldRight(lst,0)((x,y) => x + y)
  }

  def product1(lst : MyList[Int]) : Int = {
    foldRight(lst,1)((x,y) => x * y)
  }

  def length1[A](lst : MyList[Int]) : Int = {
    foldRight(lst,0)((x,y) => y+1)
  }

  def main(args: Array[String]): Unit = {
    val l1 : MyList[Int] = MyNil
    val l2 : MyList[Int] = MyCons(1,l1)
    val l3 : MyList[Int] = MyCons(2,l2)
    val l4 : MyList[Int] = MyCons(4,l3)
    val l5 : MyList[Int] = MyCons(5,l4)
    //println(length(l3))
    //println(tail(l3))
    //println(setHead(5,l3))
    //println(drop(l5,2))
    //println(dropWhile(l5,(x : Int) => x>2 ))
    //println(sum1(l5))
    //println(product1(l5))
    println(length(l5))

  }
}
