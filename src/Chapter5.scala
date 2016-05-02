/**
 * Created by rasrivastava on 4/19/16.
 */
object Chapter5 {
  def if2[A](cond : Boolean,onTrue : () => A,onFalse : () => A) :  A = {
    if (cond) onTrue()
    else onFalse()
  }

  def main(args : Array[String]) : Unit = {
    if2(false,() => println("a"),() => println("b"))
  }
}
