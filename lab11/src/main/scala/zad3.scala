import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorLogging}

case class Ball3(nextPlayer1: ActorRef, nextPlayer2: ActorRef)
case class Play3(nextPlayer1: ActorRef, nextPlayer2: ActorRef)

class Player3 extends Actor with ActorLogging{
  def receive: Receive = {
    case Play3(nextPlayer1, nextPlayer2) => nextPlayer1 ! Ball3(nextPlayer2, self)
    case Ball3(nextPlayer1, nextPlayer2) =>
      println(s"Pileczka ${self.path.name}")

      nextPlayer1 ! Ball3(nextPlayer2, self)
  }
}

@main
def lab11zad3(): Unit = {
  val system = ActorSystem("tenisStolowy")

  val player1 = system.actorOf(Props[Player3](), "player1")
  val player2 = system.actorOf(Props[Player3](), "player2")
  val player3 = system.actorOf(Props[Player3](), "player3")

  player1 ! Play3(player2, player3)
}