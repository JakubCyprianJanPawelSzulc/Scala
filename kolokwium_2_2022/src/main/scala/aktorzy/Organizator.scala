package kolokwium.actors

import akka.actor.*

object Organizator {
  case class SetMaxTick(maxTick: Int) {
    assert(maxTick > 0)
  }

  case object Tick

  case class RouteTraveled(numberOfKm: Float)
}

class Organizator extends Actor with ActorLogging {
  import Organizator._

  def receive: Receive = {
    case SetMaxTick(maxTick) =>
      val workshop = context.actorOf(Props[Warsztat](), "workshop")
      val drivers = (1 to 10).map((x)=>context.actorOf(Props(new Kierowca(workshop, 1)), s"driver$x")).toList
      for (i <- 1 to 10){
        drivers(i-1) ! Kierowca.SetupCar
      }
      context.become(gotowy(workshop, drivers, maxTick, 0))
    case Tick => {
    }
  }

  def gotowy(workshop: ActorRef, drivers: List[ActorRef], maxTick: Int, currentTick: Int): Receive={
    case Tick =>{
      if (currentTick < maxTick){
        workshop ! Warsztat.Tick
        drivers.foreach(_ ! Kierowca.Tick)
        context.become(gotowy(workshop, drivers, maxTick, currentTick + 1))
      }
      else{
        drivers.foreach(_ ! Kierowca.GetRoute)
        context.become(zWynikami(workshop, drivers, maxTick, currentTick, Nil))
      }
    }
  }

  def zWynikami(workshop: ActorRef, drivers: List[ActorRef], maxTick: Int, currentTick: Int, wyniki: List[(ActorRef, Float)]): Receive={
    case RouteTraveled(x)=>{
      val noweWyniki = (sender(), x)::wyniki
      if(noweWyniki.length == drivers.length){
        for (i <- 1 to drivers.length){
          println(s"Kierowca $i przejechal ${noweWyniki(i-1)._2} km")
        }
        context.system.terminate()
      }
      else{
        context.become(zWynikami(workshop, drivers, maxTick, currentTick, noweWyniki))
      }
    }
    case Tick =>{
    }
  }

  
}