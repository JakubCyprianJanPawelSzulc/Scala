package kolokwium.actors

import akka.actor.*

object Warsztat {
  case object Tick

  case class Malfunction(car: ActorRef)
}

class Warsztat extends Actor with ActorLogging {

  import Warsztat._

  def receive: Receive = initialized(Set(), Set())

  def initialized(
                   setOfIncomingCarsAndDrivers: Set[(ActorRef, ActorRef)],
                   setOfCarsBeingRepaired: Set[((ActorRef, ActorRef), Int)]
                 ): Receive = {
    case Malfunction(car) =>
      val driver = sender()
      context.become(
        initialized(
          setOfIncomingCarsAndDrivers.incl((driver, car)),
          setOfCarsBeingRepaired
        ))
    case Tick =>
      val randomGenerator = scala.util.Random

      setOfCarsBeingRepaired.foreach(pair => {
        val tempSetOfCarsBeingRepaired = setOfCarsBeingRepaired.excl(pair)
        val repairTicks = pair._2

        if (repairTicks - 1 == 0) {
          context.become(
            initialized(setOfIncomingCarsAndDrivers, tempSetOfCarsBeingRepaired))

          val driver = pair._1._1
          val car = pair._1._2
          driver ! Kierowca.RepairResult(Some(car))
        }

        context.become(
          initialized(
            setOfIncomingCarsAndDrivers,
            tempSetOfCarsBeingRepaired.incl((pair._1, repairTicks - 1))
          )
        )
      })

      setOfIncomingCarsAndDrivers.foreach(pair => {
        if (randomGenerator.nextFloat() * 100 < 80) {
          context.become(
            initialized(
              setOfIncomingCarsAndDrivers.excl(pair),
              setOfCarsBeingRepaired.incl((pair, randomGenerator.nextInt(6) + 1))
            )
          )
        } else pair._2 ! Kierowca.RepairResult(None)
      })

    case msg =>
  }
}