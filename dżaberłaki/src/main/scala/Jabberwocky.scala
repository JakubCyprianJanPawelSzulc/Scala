import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}
import scala.concurrent.duration._
/*
  W konfiguracji projektu wykorzystana została wtyczka
  sbt-revolver. W związku z tym uruchamiamy program poleceniem

    reStart

  a zatrzymujemy pisząc (mimo przesuwających się komunikatów)

     reStop

  i naciskając klawisz ENTER. Jeśli czynności powyższe
  już wykonywaliśmy to możemy też przywołać poprzednie
  polecenia używając strzałek góra/dół na klawiaturze.
*/


object SiłaWyższa {
  case object Cyk
  case object Strzelać
  case object Koniec
}
class SiłaWyższa extends Actor with ActorLogging {
  import SiłaWyższa._
  var zamek1 : ActorRef = _
  var zamek2 : ActorRef = _
  def receive : Receive = {
    case Cyk =>
      log.info("Cyk")
      // wysyłamy polacenie „Strzelać” do obu Zamków
      if (context.child("zamek1").isEmpty) {
        zamek1 = context.actorOf(Props(new Zamek()), "zamek1")
      }
      if (context.child("zamek2").isEmpty) {
        zamek2 = context.actorOf(Props(new Zamek()), "zamek2")
      }
      context.become(zZamkami(zamek1, zamek2))
  }
  def zZamkami(zamek1: ActorRef, zamek2: ActorRef): Receive = {
    case Cyk =>{
      log.info("Cyk")
      zamek1 ! Strzelać
      zamek2 ! Strzelać
    }
    case Koniec =>{
      log.info("Koniec")
      context.system.terminate()
    }
  }
}

class Zamek extends Actor with ActorLogging{
  def receive: Receive = {
    case SiłaWyższa.Strzelać => {
      var obroncy : Set[ActorRef] = Set.empty
      var mojaNazwa: String = ""
      val przeciwnik = if (self.path.name == "zamek1") {
        context.actorSelection("../zamek2")
      } else {
        context.actorSelection("../zamek1")
      }
      if (przeciwnik == context.actorSelection("../zamek1")) {
        mojaNazwa = "zamek2"
        log.info("Zamek2")
      } else {
        mojaNazwa = "zamek1"
        log.info("Zamek1")
      }

      if (obroncy.isEmpty) {
        for (i <- 1 to 100) {
          obroncy = obroncy + context.actorOf(Props(new Obrońca(przeciwnik)), s"${mojaNazwa}_obronca${i}")
        }
      }
      context.become(zObroncami(obroncy))
    }
  }

  def zObroncami(obroncy: Set[ActorRef]): Receive = {
    case SiłaWyższa.Strzelać => {
      log.info("Strzelac")
      obroncy.foreach(_ ! SiłaWyższa.Strzelać)
    }
    case Obrońca.Pocisk => {
      val random = scala.util.Random
      val szansa = obroncy.size.toDouble / (2 * 100)
      if (random.nextDouble() < szansa) {
        val wylosowany = random.nextInt(obroncy.size)
        val updatedObroncy = obroncy.toList.zipWithIndex.filterNot(_._2 == wylosowany).map(_._1).toSet
        obroncy.toList(wylosowany) ! Obrońca.Trafiony
        if (updatedObroncy.isEmpty) {
          log.info("Zamek zniszczony")
          context.parent ! SiłaWyższa.Koniec
          context.stop(self)
        }
        context.become(zObroncami(updatedObroncy))
      }
    }
  }
}


object Obrońca {
  case object Pocisk
  case object Trafiony
}
class Obrońca(przeciwnik: akka.actor.ActorSelection) extends Actor with ActorLogging{
  def receive: Receive = {
    case SiłaWyższa.Strzelać =>{
      przeciwnik ! Obrońca.Pocisk
    }
    case Obrońca.Trafiony =>{
      log.info("Trafiony")
      context.stop(self)
    }
  }
}

@main
def bitwa: Unit = {
  val system = ActorSystem("Jabberwocky")
  import system.dispatcher

  // UWAGA: „nazwy”, tworzące ścieżkę do aktora muszą być zapisywane
  // z użyciem znaków znaków ASCII (a nie np. UTF8) – stąd „SilaWyzsza”
  val siłaWyższa = system.actorOf(Props[SiłaWyższa](), "SilaWyzsza")

  // Do „animacji” SiłyWyższej wykorzystamy „Planistę” (Scheduler)
  val pantaRhei = system.scheduler.scheduleWithFixedDelay(
    Duration.Zero,     // opóźnienie początkowe
    1000.milliseconds, // odstęp pomiedzy kolejnymi komunikatami
    siłaWyższa,        // adresat „korespondencji”
    SiłaWyższa.Cyk     // komunikat
  )

  // Oczywiście zatrzymanie symulacji NIE MOŻE się odbyć tak, jak poniżej
  // Thread.sleep(3000)
  // val res = if pantaRhei.cancel()
  //   then "Udało się zakończyć „cykanie”"
  //   else "Coś poszło nie tak – dalej „cyka”"
  // println(res)
  // system.terminate()
}
