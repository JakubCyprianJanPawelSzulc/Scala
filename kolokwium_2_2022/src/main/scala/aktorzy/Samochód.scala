package kolokwium.actors

import akka.actor._

object Samochód {
  case object Next
}

class Samochód extends Actor with ActorLogging {
  import Samochód._

  def receive: Receive = {
    case Next =>
      val randomGenerator = scala.util.Random

      if (randomGenerator.nextFloat * 100 > 15) {
        val speed = randomGenerator.nextInt(200)

        sender() ! Kierowca.CarReaction(Some(speed))
      } else sender() ! Kierowca.CarReaction(None)
  }
}