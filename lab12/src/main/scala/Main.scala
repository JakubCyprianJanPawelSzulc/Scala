import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

class Boss extends Actor with ActorLogging {
  def receive : Receive ={
    case Oblicz(n) => {
      val nadzorca = context.actorOf(Props(new Nadzorca()), "kierownik")
      nadzorca ! Oblicz(n)
      context.become(zKierownikiem(nadzorca))
    }
  }
  def zKierownikiem(nadzorca: ActorRef): Receive = {
    case Oblicz(n) => {
      nadzorca ! Oblicz(n)
    }
    case Wynik(n, fib) => {
      log.info(s"fib($n) = $fib")
    }
  }
}

case class Oblicz(n: Int)

class Nadzorca(cache: Map[Int, BigInt] = Map(1 -> 1, 2 -> 1), doZrobienia: Set[Int] = Set()) extends Actor with ActorLogging {
  def receive : Receive ={
    case Oblicz(n) => {
      if(cache.contains(n)){
        log.info(s"$n już było obliczone")
        sender() ! Wynik(n, cache(n))
      }else{
        val pracownik = context.actorOf(Props(new Pracownik(n)), s"pracownik$n")
        pracownik ! Oblicz(n)
        context.become(zPracownikiem(pracownik, cache, doZrobienia))
      }
    }
  }
  def zPracownikiem(pracownik: ActorRef, cache: Map[Int, BigInt], doZrobienia: Set[Int]): Receive = {
    case Oblicz(n) => {
      if(cache.contains(n)){
        log.info(s"$n już było obliczone")
        sender() ! Wynik(n, cache(n))
      }else{
        pracownik ! Oblicz(n)
      }
    }
    case Wynik(n, fib) =>{
      val nowyCache = cache + (n -> fib)
      context.parent ! Wynik(n, fib)
      context.become(zPracownikiem(pracownik, nowyCache, doZrobienia))
    }
    
  }
  
}

case class Wynik(n: Int, fib: BigInt)

class Pracownik(k: Int) extends Actor with ActorLogging {
  def receive: Receive ={
    case Oblicz(n) => {
      val wynik = fib(n)
      sender() ! Wynik(n, wynik)
    }
  }
  def fib (n: Int): BigInt = {
      if(n == 1 || n == 2){
        1
      }else{
        fib(n-1) + fib(n-2)
      }
    }
}

@main
def dupajaja : Unit = {
  val system = ActorSystem("Fibonacci")
  val kierownik = system.actorOf(Props(new Boss()), "kierownik")
  kierownik ! Oblicz(12)
  kierownik ! Oblicz(2)
  kierownik ! Oblicz(3)
  Thread.sleep(500)
  kierownik ! Oblicz(12)

}