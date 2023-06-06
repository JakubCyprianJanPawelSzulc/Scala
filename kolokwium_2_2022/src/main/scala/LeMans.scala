package kolokwium

import actors.Organizator
import akka.actor._
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

@main
def race(): Unit = {
  val system = ActorSystem("LeMans")
  import system.dispatcher

  val MaxTick = 300 // liczba „cyknięć” do wykonania (można eksperymentować)

  // UWAGA: „nazwy”, tworzące ścieżkę do aktora muszą być zapisywane
  // z użyciem znaków ASCII (a nie np. UTF8)!
  val organiser = system.actorOf(Props[Organizator](), "organiser")
  organiser ! Organizator.SetMaxTick(MaxTick)

  // Do „animacji” Organizatora wykorzystamy „Planistę” (Scheduler)
  val pantaRhei = system.scheduler.scheduleWithFixedDelay(
    Duration.Zero,      // opóźnienie początkowe
    10.milliseconds,    // rzeczywisty odstęp pomiędzy kolejnymi „cyknięciami”
    organiser,        // adresat „korespondencji”
    Organizator.Tick     // komunikat
  )
}