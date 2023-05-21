import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorLogging}
import scala.io.StdIn
import scala.util.Random

case object Piłeczka4b

case class Graj04b(tabprzeciwnik : IndexedSeq[akka.actor.ActorRef])

var rand = new Random()
var random : Int = _
var newRandom : Int = _ 
var tab2 : IndexedSeq[akka.actor.ActorRef] = _

class Gracz04b extends Actor with ActorLogging{
   def receive : Receive = {
       case Graj04b(tablica) => {
          tab2 = tablica
          random = rand.nextInt(tab2.length)
          tab2(random) ! Piłeczka
       }
       case Piłeczka => {
          println(s"Odbija ${self}")
          newRandom = rand.nextInt(tab2.length)
          while(newRandom == random){
              newRandom = rand.nextInt(tab2.length)
          } 
          random = newRandom
          tab2(random) ! Piłeczka
       }
   }
}

@main
def lab11zad4b() = {
    val system = ActorSystem("system")
    val gracz1 = system.actorOf(Props[Gracz04b](), "gracz1")
    val gracz2 = system.actorOf(Props[Gracz04b](), "gracz2")
    val gracz3 = system.actorOf(Props[Gracz04b](), "gracz3")
    val gracz4 = system.actorOf(Props[Gracz04b](), "gracz4")
    val gracz5 = system.actorOf(Props[Gracz04b](), "gracz5")

    gracz1 ! Graj04b(IndexedSeq(gracz1, gracz2, gracz3, gracz4, gracz5))
}
