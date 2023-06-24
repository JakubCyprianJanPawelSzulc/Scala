package kolokwium_2

import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

private def dane: List[String] = {
  import scala.io.Source
  val plik = Source.fromFile("src/main/resources/dane.txt", "UTF-8")
  plik
    .getLines.toList
    .flatMap { linia => linia.split("[^\\p{IsAlphabetic}]+").toList }
}

@main def main: Unit = {
  val system = ActorSystem("sys")
  val szef = system.actorOf(Props[Szef](), "szef")
  dane.foreach { słowo => szef ! W(słowo) }

  def askUser(): Unit = {
    Thread.sleep(100)
    println("Co chcesz zrobic?")
    println("1. Wstaw slowo")
    println("2. Policz ile razy wystepuje slowo")
    println("3. Zakoncz")
    val input = scala.io.StdIn.readInt()
    input match {
      case 1 => {
        println("Podaj slowo")
        val słowo = scala.io.StdIn.readLine()
        szef ! W(słowo)
        askUser()
      }
      case 2 => {
        println("Podaj slowo")
        val słowo = scala.io.StdIn.readLine()
        szef ! I(słowo)
        askUser()
      }
      case 3 => {
        system.terminate()
      }
      case _ => {
        println("Niepoprawna komenda")
        askUser()
      }
    }
  }
  askUser()
}
