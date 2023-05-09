import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorLogging}
import scala.io.StdIn

case class Ball4(players: List[ActorRef], playersNumber: Int)
case class Play4(players: List[ActorRef], playersNumber: Int)

class Player4 extends Actor with ActorLogging{
  def receive: Receive = {
    case Play4(players, playersNumber) => players(playersNumber) ! Ball4(players, playersNumber)
    case Ball4(players, playersNumber) =>
      println(s"Pileczka ${self.path.name}")

      if (playersNumber + 1 > players.length - 1) players.head ! Ball4(players, 0)
      else players(playersNumber + 1) ! Ball4(players, playersNumber + 1)
  }
}

@main
def lab11zad4a(): Unit = {
  val system = ActorSystem("tenisStolowy")

  println("Podaj liczbe graczy: ")
  val numberOfPlayers = StdIn.readLine().toInt

  val listOfPlayers = List.range(0, numberOfPlayers).map(playersNumber => {
    system.actorOf(Props[Player4](), s"player${playersNumber + 1}")
  })

  listOfPlayers.head ! Play4(listOfPlayers, 0)
}