

import akka.actor.Actor.Receive
import akka.actor.{Actor, Props, ActorSystem}


/**
 * Created by Rahul on 4/19/16.
 */
class HelloActor extends Actor {
  override def receive: Receive = {
    case "incr" => {
      println("incr " + counter)
      counter += 1
    }


    case "get" => sender() ! counter

  }

  var counter = 0
}
class actors extends Actor {


  val counter = context.actorOf(Props[HelloActor], name = "helloactor")
  counter ! "incr"
  counter ! "incr"
  counter ! "get"



  override def receive: Actor.Receive = {
    case n: Int => println(n)
      context.stop(self)
  }
}
