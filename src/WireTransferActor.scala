import akka.actor.Actor.Receive
import akka.actor.{Props, Actor, ActorRef}
import akka.actor.Actor.Receive
import akka.remote.transport.ThrottlerTransportAdapter.Direction.Receive
import BankAccount._

/**
 * Created by Rahul on 4/21/16.
 */
class WireTransferActor extends Actor {
  import WireTransfer._
  override def receive: Receive = {
    case Transfer(from : ActorRef,to : ActorRef,amount : BigInt) => {
      from ! BankAccount.Withdraw(amount)
      context.become(awaitWithDraw(to,amount,sender()))
    }
  }

  def awaitWithDraw(to : ActorRef,amount : BigInt,client : ActorRef) : Receive = {
    case BankAccount.Done =>
      to ! BankAccount.Deposit(amount)
      context.become(awaitDeposit(client))
    case BankAccount.Failed =>
      client ! Failed
      context.stop(self)
  }

  def awaitDeposit(client : ActorRef) : Receive = {
    case BankAccount.Done =>
      client ! Done
      context.stop(self)
  }
}

class WireTransferMain extends Actor {

  val acc1 = context.actorOf(Props[BankActor],"account1")
  val acc2 = context.actorOf(Props[BankActor],"account2")

  acc1 ! Deposit(100)
  override def receive: Receive = {
    case Done =>
      transfer(acc1,acc2,50)
  }

  def transfer(source : ActorRef,dest : ActorRef,amount : Int) : Unit = {
    var wireTransfer = context.actorOf(Props[WireTransferActor],"wireTransferTxn")
    wireTransfer ! WireTransfer.Transfer(source,dest,amount)
    context.become( {
      case WireTransfer.Done =>
        println("success")
        context.stop(self)
      case WireTransfer.Failed =>
        println("failed")
        context.stop(self)
    })
  }
}




