import java.util.Optional

/**
 * Created by Rahul on 4/19/16.
 */

trait MyOption[+A] {
  def map[B](f: A => B) : MyOption[B]

  def flatMap[B](f : A => MyOption[B]) : MyOption[B]

  def getOrElse[B >: A] (default : => B) : B = {
    default
  }

  def orElse[B >: A] (ob : => MyOption[B]) : MyOption[B] = {
    ob
  }

  //def filter(f : A => Boolean) : MyOption[A]
}
case class SomeOption[+A](get : A) extends MyOption[A] {
  override def map[B](f: (A) => B): MyOption[B] = {
    SomeOption[B](f(get))
  }

  override def flatMap[B](f: (A) => MyOption[B]): MyOption[B] = {
    f(get)
  }

  override def getOrElse[B >: A](default: => B): B = {
    get
  }

  override def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    SomeOption[B](get)
  }

//  override def filter(f: (A) => Boolean): MyOption[A]
//
//  }
//  override def filter(f: (A) => Boolean): MyOption[A] = {
//     if (f(get) == false) {
//       SomeOption[A](get)
//     } else {
//       NoneOption
//     }
//
//  }
  //override def filter(f: (A) => Boolean): MyOption[A] = ???
}

case object NoneOption

sealed trait MyEither[+E,+A] {
  def map[B](f: A => B) : MyEither[E,B]
  //def flatMap[EE >: E,B](f: A => MyEither[EE,B]) : MyEither[EE,B]
}
case class MyLeft[+E](value : E) extends MyEither[E,Nothing] {
  override def map[B](f: (Nothing) => B): MyEither[E, B] = {
    MyLeft(value)
  }
}
case class MyRight[+A](value : A) extends MyEither[Nothing,A] {
  override def map[B](f: (A) => B): MyEither[Nothing, B] = {
    MyRight(f(value))
  }
}

object MainClass {


  def variance(xs : List[Double]) : Double = {
    def mean(xs : List[Double]) : Double = {
      xs.sum/xs.length
    }


    def getValue(x : Double) : Double = {
      math.pow(x-mean(xs),2)
    }

    val updatedLst = xs.map(getValue)
    mean(updatedLst)


  }

  def mean(lst : List[Double]) : Option[Double] = {
    Some(lst.sum/lst.length)

  }
  def varianceWithOption(xs : List[Double]) : Option[Double] = {


    val m = mean(xs)
    Some(xs.flatMap(x => List(math.pow(x-m.get,2))).sum/xs.length)
  }


  def lift[A,B](f: A => B) : Option[A] => Option[B] = {
    x => x.map(f)
  }

  def absO : Option[Double] => Option[Double] = lift(math.abs)

  def map2[A,B,C](a : Option[A], b: Option[B]) (f : (A,B) => C) : Option[C] = {
    a.flatMap(x => {
      b.map(y =>{
        f(x,y)
      })
    })
  }

  def sequence[A](a : List[Option[A]]) : Option[List[A]] = {
    val l = a.filter(_ != None)
    if (l.length != a.length) {
      None
    }else {
      Some(l.map(x => x.get))
    }
  }

  def traverse[A,B](a : List[A]) (f : A => Option[B]) : Option[List[B]] = {
    def traverseInternal(aa : List[A]) : List[B] = {
      aa match {
        case Nil => Nil
        case x::xs => {
          val z = f(x)
          z match {
            case None => traverseInternal(xs)
            case _ => (z.get)::traverseInternal(xs)
          }
        }

      }
    }

    Some(traverseInternal(a))
  }

  ///Either data type functions
  def mean(xs : IndexedSeq[Double]) : MyEither[String,Double] = {
    if (xs.isEmpty) {
      MyLeft("Empty")
    }else {
      MyRight(xs.sum/xs.length)
    }
  }

  def safeDiv(x : Int,y: Int) : MyEither[Exception,Int]  = {
    try MyRight(x/y)
    catch {
      case e : Exception => MyLeft(e)
    }
  }
  def main(args : Array[String]): Unit = {
    //failingFunction(10)
    //var s = Seq(1.0,12.0,2.0)
    print(varianceWithOption(List(1,2,3)))
    println(List())
    val l = List(Option(1),Option(2),Option(3),None)
    val l1 = List(None)
    println(sequence(l))
    //println(sequence(List(Option(1),Option(2),Option(3),None)))
//    var r= mean(s)
//    r match {
//      case None => println("empty")
//      case Some(v) => println(v)
//    }
  }

}
