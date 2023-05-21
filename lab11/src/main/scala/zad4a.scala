import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorLogging}
import scala.io.StdIn

case object Piłeczka4a

case class Graj04a(tabprzeciwnik : IndexedSeq[akka.actor.ActorRef])

var tab : IndexedSeq[akka.actor.ActorRef] = _

class Gracz04a extends Actor with ActorLogging{
   def receive : Receive = {
       case Graj04a(tablica) => {
           tab = tablica
           tab.head ! Piłeczka
       }
       case Piłeczka => {
           println(s"Odbija ${self}")
           tab = tab.tail :+ tab.head
           tab.head ! Piłeczka
       }
   }
}

@main
def lab11zad4a() = {
    val system = ActorSystem("system")
    val gracz1 = system.actorOf(Props[Gracz04a](), "gracz1")
    val gracz2 = system.actorOf(Props[Gracz04a](), "gracz2")
    val gracz3 = system.actorOf(Props[Gracz04a](), "gracz3")
    val gracz4 = system.actorOf(Props[Gracz04a](), "gracz4")
    val gracz5 = system.actorOf(Props[Gracz04a](), "gracz5")

    gracz1 ! Graj04a(IndexedSeq(gracz1, gracz2, gracz3, gracz4, gracz5))
}
