import akka.actor.{Actor, ActorLogging, ActorSystem, ActorRef, Props}
import scala.io.StdIn
import scala.util.Random

case class Ball4b(players: List[ActorRef], playersNumber: Int)
case class Play4b(gracze: List[ActorRef])

class Player4b extends Actor with ActorLogging {
  def receive: Receive = {
    case Play4b(gracze) =>
      val pierwszyGracz = gracze(Random.nextInt(gracze.length))
      pierwszyGracz ! Ball4b(gracze, gracze.indexOf(self))
    case Ball4b(gracze, playersNumber) =>
      println(s"Pilka u ${self.path.name}")
      val nastepnyGracz = gracze(Random.nextInt(gracze.length))
      nastepnyGracz ! Ball4b(gracze, gracze.indexOf(self))
  }
}

@main
def lab11zad4b(): Unit = {
  val system = ActorSystem("tenisStolowy")

  println("Podaj liczbe graczy: ")
  val numberOfPlayers = StdIn.readLine().toInt

  val listOfPlayers = List.range(0, numberOfPlayers).map(playersNumber => {
    system.actorOf(Props[Player4b](), s"player${playersNumber + 1}")
  })

  listOfPlayers.foreach(gracz => gracz ! Play4b(listOfPlayers))
}
