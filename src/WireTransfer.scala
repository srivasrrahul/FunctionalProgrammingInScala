import akka.actor.ActorRef

/**
 * Created by Rahul on 4/21/16.
 */
object WireTransfer {
  case class Transfer(from : ActorRef,to : ActorRef,amount : BigInt)
  case object Done
  case object Failed
}
