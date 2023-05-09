import akka.actor.{Actor, ActorLogging, ActorRef, Props, ActorSystem}

object Ball1
case class Play1(opponent: ActorRef)

class Player1 extends Actor with ActorLogging {

  def receive: Receive = {
    case Play1(actor) =>
      log.info("Rozpoczynamy grę!")
      actor ! Ball1
    case Ball1 =>
      log.info(s"Otrzymalem pileczke od ${sender().path.name}, odsylam ja z powrotem")
      sender() ! Ball1
    case _ =>
      log.warning("Nieznany komunikat")
  }
}

@main
def lab11zad1(): Unit = {
  val system = ActorSystem("tenisStolowy")
  val player1 = system.actorOf(Props[Player1](), "player1")
  val player2 = system.actorOf(Props[Player1](), "player2")

  player1 ! Play1(player2)
}
