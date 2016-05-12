/**
 * Created by Rahul on 4/19/16.
 */
sealed trait Stream[+A] {
  def toList: List[A]
  def take(n: Int) : List[A]
  def takeStream(n : Int) : Stream[A]
  def drop(n : Int) : Stream[A]
  def takeWhile(predicate : A => Boolean) : Stream[A]
  def forAll(predicate : A => Boolean) : Boolean
  def foldRight[B](z : => B)(f : (A, => B) => B) : B
  def takeWhileUsingFold(predicate : A => Boolean) : Stream[A]
  def headOptionUsingFoldRight() : Option[A]
  def map[B](transform : A => B) : Stream[B]
  def filter(transform : A => Boolean) : Stream[A]
  def flatMap[B >: A](transform : A => Stream[B]) : Stream[B]
  def append[B >: A](lst : Stream[B]) : Stream[B]


}
case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = {
    Nil
  }

  override def take(n: Int): List[Nothing] = {
    Nil
  }

  override def takeStream(n: Int): Stream[Nothing] = {
    Empty
  }

  override def drop(n: Int): Stream[Nothing] = Empty

  override def takeWhile(predicate: (Nothing) => Boolean): Stream[Nothing] = Empty

  override def forAll(predicate: (Nothing) => Boolean): Boolean = {
    false
  }

  override def foldRight[B](z : => B)(f : (Nothing, => B) => B) : B = z

  override def takeWhileUsingFold(predicate: (Nothing) => Boolean): Stream[Nothing] = Empty

  override def headOptionUsingFoldRight(): Option[Nothing] = None

  override def map[B](transform: (Nothing) => B): Stream[B] = Empty

  override def filter(transform: (Nothing) => Boolean): Stream[Nothing] = Empty

  override def flatMap[B >: Nothing](transform: (Nothing) => Stream[B]): Stream[B] = Empty

  override def append[B >: Nothing](lst: Stream[B]): Stream[B] = lst
}

case class Cons[+A](h : () => A,t : () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = {
    val lst : List[A] = t().toList
    h()::lst
  }

  override def take(n: Int): List[A] = {

    if (n == 0) Nil
    else {
      val lst : List[A] = t().take(n-1)
      h()::lst
    }

  }

  override def takeStream(n: Int): Stream[A] = {
    if (n == 0) Empty
    else {
      val v : Stream[A] = t().takeStream(n-1)
      Cons(h,() => v)


    }
  }

  override def drop(n: Int): Stream[A] = {
    if (n == 0) {
      t()
    }
    else {
      t().drop(n-1)
    }
  }

  override def takeWhile(predicate: (A) => Boolean): Stream[A] = {
    if (predicate(h())) {
      Cons(h,() => t().takeWhile(predicate))
    }else {
      t().takeWhile(predicate)
    }
  }

  override def forAll(predicate: (A) => Boolean): Boolean = {
    if (predicate(h())) {
      true
    } else {
      t().forAll(predicate)
    }
  }

//  override def foldRight[B](z: => B)(f: (A, B) => B): B = {
//    f(h(),t().foldRight(z)(f))
//  }
  override def foldRight[B](z : => B)(f : (A, => B) => B) : B = {
      f(h(),t().foldRight(z)(f))
  }

  override def takeWhileUsingFold(predicate: (A) => Boolean): Stream[A] = {
    def internalFunc(x : A,y : => Stream[A]) : Stream[A] = {
      if (predicate(x)) {
        Stream.cons( x,  y)
      } else {
        Empty
      }
    }
    foldRight(Empty : Stream[A])(internalFunc)
  }

  //Wow hard function
  override def headOptionUsingFoldRight(): Option[A] = {
    def internalFunc(x : A,y: => Option[A]) : Option[A] = {
      Some(x)
    }


    foldRight[Option[A]](Some(h()))(internalFunc)
  }

//  override def map[B](transform : (=> A) =>( => B)): Stream[B] = {
//    def internalFunc(x : A, y: => Stream[B]) : Stream[B] = {
//      Cons(() => transform(x),() => y)
//    }
//
//    foldRight(Empty : Stream[B])(internalFunc)
//  }
  override def map[B](transform: A => B): Stream[B] = {
      def internalFunc(x : A, y:  => Stream[B]) : Stream[B] = {
        Cons(() => transform(x),() => y)
      }

      foldRight(Empty : Stream[B])(internalFunc)
  }

  override def filter(predicate : (A) => Boolean): Stream[A] = {
    def internalFunc(x : A, y:  => Stream[A]) : Stream[A] = {
      if (predicate(x)) {
        Cons(() => x,() => y)
      }else {
        y
      }
    }
    foldRight(Empty : Stream[A])(internalFunc)

  }

  override def flatMap[B >: A](transform: A => Stream[B]): Stream[B] = {
    def internalFunc(x : A, y:  => Stream[B]) : Stream[B] = {
      Cons(() => transform(x).toList.head,() => y)
    }
    foldRight(Empty : Stream[B])(internalFunc)
  }

  override def append[B >: A](lst: Stream[B]): Stream[B] = {
    def internalFunc(x : A, y:  => Stream[B]) : Stream[B] = {
      Cons(() => x,() => y)
    }

    foldRight(lst)(internalFunc)
  }
}

object Stream {
  def cons[A](hd : => A,tl : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head,() => tail)
  }

  def empty[A] : Stream[A] = Empty

  def constant[A](a : A) : Stream[A] = {
    lazy val constLst : Stream[A]= cons(a,constLst)
    constLst
  }

  def from(n: Int) : Stream[Int] = {

    lazy  val incLst : Stream[Int] = cons(n,from(n+1))
    incLst
  }

  def fibs() : Stream[Int] = {
    def fibsInternal(x : Int,y : Int) : Stream[Int] = {
      cons(x+y,fibsInternal(y,x+y))
    }
    lazy val fibLst : Stream[Int] = fibsInternal(0,1)
    fibLst
  }

  def unfold[A,S](z: S)(f : S => Option[(A,S)]) : Stream[A] = {


    lazy val nextState : Option[(A,S)]= f(z)
    def internal(): Stream[A] = {
      nextState match {
        case Some(x) => cons(nextState.get._1,unfold(nextState.get._2)(f))
        case None => Empty
      }

    }

    internal
  }

  def fibsUsingUnfold() : Stream[Int] = {
    def fibsInternal(x : (Int,Int)) : Option[(Int,(Int,Int))] = {
      Some(x._1+x._2,(x._2,x._1+x._2))
    }

    unfold[Int,(Int,Int)](0,1)(fibsInternal)

  }

  def fromUsingUnfold(n : Int) : Stream[Int] = {
    def internal(n : Int) : Option[(Int,Int)] = {
      Some(n+1,n+1)
    }
    unfold[Int,Int](n)(internal)
  }

  def constantUsingFold[A](n : A) : Stream[A] = {
    def internal(n : A) : Option[(A,A)] = {
      Some(n,n)
    }
    unfold[A,A](n)(internal)
  }

  def mapUsingFold[A](stream : Stream[A])(f : A => A) : Stream[A] = {
    def internal(x : Stream[A]) : Option[(A,Stream[A])] = {
      x match {
        case Cons(y,z) => Some(f(y()),z())
        case Empty => None
      }
    }
    unfold[A,Stream[A]](stream)(internal)
  }

  def takeUsingFold[A](stream : Stream[A],n : Int) : Stream[A] = {

    def internal(x : Stream[A]) : Option[(A,Stream[A])] = {
      x match {
        case Cons(y,z) => {
          if (n > 0) {
            Some(y(),takeUsingFold(z(),n-1))
          }else {
            None
          }
        }
        case Empty => None
      }
    }
    unfold[A,Stream[A]](stream)(internal)
  }

  def takeWhileUsingFold[A](stream : Stream[A])(predicate : A => Boolean) : Stream[A] = {

    def internal(x : Stream[A]) : Option[(A,Stream[A])] = {
      x match {
        case Cons(y,z) => {
          lazy val yVal = y()
          if (predicate(yVal)) {
            Some(yVal,takeWhileUsingFold(z())(predicate))
          }else {
            None
          }
        }
        case Empty => None
      }
    }
    unfold[A,Stream[A]](stream)(internal)
  }

  //Assume both are of same length
  def zipWith[A,B,C](s1 : Stream[A],s2 : Stream[B])(f : (A,B) => C) : Stream[C] = {

    def internal(x : Stream[A],y : Stream[B]) : Option[(C,Stream[C])] = {
      x match {
        case Cons(x1,x2) =>
          y match {
            case Cons(y1,y2) =>
          }
      }
    }

    unfold[C,Stream[C]]()
  }
//  def apply[A] (as : Stream[A])  : Stream[A]  = {
//    as match {
//      case Empty => Empty
//      case Cons(hd,tl) => Cons(hd,apply(tl))
//    }
//  }

  def headOption[A] (as : Stream[A]) : Option[A] = {
    as match {
      case Empty => None
      case Cons(hd,tl) => Some(hd())
    }
  }



}
object Chapter5 {
  def if2[A](cond : Boolean,onTrue : () => A,onFalse : () => A) :  A = {
    if (cond) {
      onTrue()

    }
    else onFalse()
  }

  def maybeTwice(b : Boolean, i: => Int) = {
    lazy val x = i
    if (b) {
     x
    }else {
      0
    }
  }
  def main(args : Array[String]) : Unit = {
    if2(true,() => println("a"),() => println("b"))
    maybeTwice(true,{println("exc "); 1+1 })
    val emptyLst = Stream.empty
    val l = Stream.cons(1,emptyLst)
    val l1 = Stream.cons(2,  l)
    val l2 = Stream.cons(3, l1)
    println(l2.headOptionUsingFoldRight())
    println(l2.map[Int](x => 2*x).headOptionUsingFoldRight())
    println(l2.flatMap(x => Stream.cons(3*x,emptyLst)).headOptionUsingFoldRight())
    println(l2.flatMap(x => Stream.cons(3*x,emptyLst)).append(l2).toList)
    lazy val ones : Stream[Int] = Stream.cons(1,ones : Stream[Int])
    println(ones.take(3).toList)
    println(Stream.constant(10).take(10).toList)
    println(Stream.from(10).take(10).toList)
    println("test")
    println("tyest" + Stream.constantUsingFold(20).take(10).toList)
    println(Stream.mapUsingFold(l2)(3*_).take(4).toList)
    println(Stream.takeWhileUsingFold(l2)(_ > 1).toList)
//    val infinite : Stream[Int] = Stream.cons(1, infinite : Stream[Int])
//    //println(ones.take(3).toList)
//    //println(ones.takeWhile(_ == 1))
  }
}
