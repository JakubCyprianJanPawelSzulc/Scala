import akka.actor.{Actor, ActorLogging, ActorRef, Props, ActorSystem}

case object Piłeczka
case class Graj01(przeciwnik: ActorRef)

class Gracz01 extends Actor with ActorLogging {
    def receive: Receive = {
        case Graj01(przeciwnik) =>
            println(s"Zaczynamy gre, pierwsze odbicie ${self}")
            przeciwnik ! Piłeczka
        case Piłeczka =>
            println(s"Wlasnie odbija ${self}")
            sender() ! Piłeczka
    }
}

@main
def lab11zad1() = {
    val system = ActorSystem("system")
    val gracz1 = system.actorOf(Props[Gracz01](), "gracz1")
    val gracz2 = system.actorOf(Props[Gracz01](), "gracz2")
    gracz1 ! Graj01(gracz2)
}