package kolokwium.actors

import akka.actor._

object Kierowca {
  case object Tick

  case object SetupCar

  case class CarReaction(ov: Option[Int])

  case object GetRoute

  case class RepairResult(effect: Option[ActorRef])
}

class Kierowca(workshop: ActorRef, dt: Int) extends Actor with ActorLogging {
  import Kierowca._

  def receive: Receive = {
    case SetupCar =>{
      val car = context.actorOf(Props[Samochód](), "car")
      context.become(gotowy(car, 0))
    }
    case Tick =>{
      println("Kierowca nie ma samochodu")
    }
    case GetRoute =>{
      println("Kierowca nie ma samochodu")
    }
  }

  def gotowy(car: ActorRef, route: Float): Receive={
    case Tick =>{
      car ! Samochód.Next
    }
    case CarReaction(ov) =>{
      if (ov == None){
        workshop ! Warsztat.Malfunction(car)
        context.become(naprawa(car, route))
      }
      else{
        val v = ov.get
        val dt2 = dt.toFloat
        val s = route + (dt2/60)*v
        context.become(gotowy(car, s))
      }
    }
    case GetRoute => {
      sender() ! Organizator.RouteTraveled(route)
    }
  }

  def naprawa(car: ActorRef, route: Float): Receive={
    case RepairResult(effect) =>{
      if (effect == None){
        context.become(samochodZepsutyCalkowicie(car, route))
      }
      else if (effect.get == car){
        context.become(gotowy(car, route))
      }
    }
    case GetRoute =>{
      sender() ! Organizator.RouteTraveled(route)
    }
    case Tick =>{
    }
  }

  def samochodZepsutyCalkowicie(car: ActorRef, route: Float): Receive={
    case GetRoute =>{
      sender() ! Organizator.RouteTraveled(route)
    }
    case _ =>{}
  }
}