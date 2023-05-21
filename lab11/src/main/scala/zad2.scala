import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging}

case object Piłeczka2
case class Play2(opponent: ActorRef, max: Int)

var aktualnaliczba = 0
var maksymalnaliczba = 0

class Player2 extends Actor with ActorLogging {
    def receive: Receive = {
        case Play2(opponent, liczba) =>
            maksymalnaliczba = liczba
            println(s"Zaczynamy gre, pierwsze odbicie ${self}")
            aktualnaliczba += 1
            opponent ! Piłeczka2
        case Piłeczka2 =>
            if(aktualnaliczba<maksymalnaliczba){
                println(s"Właśnie odbija ${self}")
                aktualnaliczba +=1
                sender() ! Piłeczka2
            }else{
                println(s"Gra zakończyła się po ${aktualnaliczba} odbiciach")
                context.system.terminate()
            }
    }
}

@main
def lab11zad2() = {
    val system = ActorSystem("system")
    val gracz1 = system.actorOf(Props[Player2](), "gracz1")
    val gracz2 = system.actorOf(Props[Player2](), "gracz2")
    gracz1 ! Play2(gracz2, 10)
}