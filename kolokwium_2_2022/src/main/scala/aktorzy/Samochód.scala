package kolokwium.actors

import akka.actor._

object Samochód {
  case object Next
}

class Samochód extends Actor with ActorLogging {
  import Samochód._

  def receive: Receive = {
    case Next =>{
      val random = scala.util.Random
      val x = random.nextInt(100)
      if(x<=10){
        sender() ! Kierowca.CarReaction(None)
      }
      else{
        val v = random.nextInt(200)
        sender() ! Kierowca.CarReaction(Some(v))
      }
    }
  }
}