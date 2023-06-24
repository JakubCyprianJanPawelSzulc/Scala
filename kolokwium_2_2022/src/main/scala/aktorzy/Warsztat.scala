package kolokwium.actors

import akka.actor.*

object Warsztat {
  case object Tick

  case class Malfunction(car: ActorRef)
}

class Warsztat extends Actor with ActorLogging {

  import Warsztat._

  def receive : Receive ={
    case Tick=>{

    }
    case Malfunction(car)=>{
      val random = scala.util.Random
      val x = random.nextInt(100)
      if (x <= 80){
        sender() ! Kierowca.RepairResult(None)
      }
      else{
        val random = scala.util.Random
        val time = random.nextInt(6) + 1
        context.become(zSamochodami(List((car, sender(), time))))
      }
    }
  }
  def zSamochodami(samochody: List[(ActorRef, ActorRef, Int)]): Receive = {
    case Tick =>{
      val noweSamochody = samochody.map((x)=>(x._1, x._2, x._3 - 1))
      val repaired = noweSamochody.filter((x)=>x._3 == 0)
      repaired.foreach((x)=>x._2 ! Kierowca.RepairResult(Some(x._1)))
      val notRepaired = noweSamochody.filter((x)=>x._3 != 0)
      context.become(zSamochodami(notRepaired))
    }
    case Malfunction(car)=>{
      val random = scala.util.Random
      val x = random.nextInt(100)
      if (x <= 80){
        sender() ! Kierowca.RepairResult(None)
      }
      else{
        val random = scala.util.Random
        val time = random.nextInt(6) + 1
        context.become(zSamochodami((car, sender(), time)::samochody))
      }
    }
  }
}