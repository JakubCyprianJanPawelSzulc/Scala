import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorLogging}

case object Piłeczka3
case class Graj03(gracz1: ActorRef, gracz2: ActorRef, gracz3: ActorRef)

var g1 : akka.actor.ActorRef = _
var g2 : akka.actor.ActorRef = _
var g3 : akka.actor.ActorRef  = _

class Gracz03 extends Actor with ActorLogging {
    def receive: Receive = {
        case Graj03(gracz1, gracz2, gracz3) =>
            g1 = gracz1
            g2 = gracz2
            g3 = gracz3
            println(s"Zaczynamy gre, pierwsze odbicie ${self}")
            context.become(granie(gracz1, gracz2, gracz3))
            g2 ! Piłeczka
        case Piłeczka => {
            if(self == g2){
                context.become(granie(g1,g2,g3))
                g3 ! Piłeczka
            }else{
                context.become(granie(g1,g2,g3))
                g1 ! Piłeczka
            }
        }
  }

    def granie(gracz1: ActorRef, gracz2: ActorRef, gracz3 : ActorRef): Receive = {
        case Piłeczka =>
            if (self == gracz1){
                println(s"Wlasnie odbija ${self}")
                gracz2 ! Piłeczka
            }else if(self == gracz2){
                println(s"Wlasnie odbija ${self}")
                gracz3 ! Piłeczka 
            }else{
                println(s"Wlasnie odbija ${self}")
                gracz1 ! Piłeczka
            }
    }
}

@main
def lab11zad3() = {
    val system = ActorSystem("system")
    val gracz1 = system.actorOf(Props[Gracz03](), "gracz1")
    val gracz2 = system.actorOf(Props[Gracz03](), "gracz2")
    val gracz3 = system.actorOf(Props[Gracz03](), "gracz3")
    gracz1 ! Graj03(gracz1, gracz2, gracz3)
}