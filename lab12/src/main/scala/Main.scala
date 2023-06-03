import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

class Boss extends Actor with ActorLogging{
  var nadzorca : ActorRef = _
  def receive: Receive = {
    case Oblicz(x)=>{
      if(context.child("nadzorca").isEmpty){
        nadzorca = context.actorOf(Props(new Nadzorca()), "nadzorca")
      }
      nadzorca ! Oblicz(x)
    }
    case Wynik(x, wynik)=>{
      log.info(s"${x} ist gleich ${wynik}")
    }
  }
}

case class Oblicz(n: Int)

class Nadzorca(var cache: Map[Int, BigInt] = Map(1 -> 1, 2 -> 1), doZrobienia: Set[Int] = Set()) extends Actor with ActorLogging {
  var pracownik : ActorRef = _
  def receive: Receive ={
    case Oblicz(x) =>{
      if(context.child("pracownik").isEmpty){
        pracownik = context.actorOf(Props(new Pracownik(x)), "pracownik")
      }
      if(cache.contains(x)){
        log.info(s"Dieses Element ${x} wurde bereits berechnet")
        context.parent ! Wynik(x, cache(x))
      }else{
        pracownik ! Oblicz(x)
      }
    }
    case Wynik(x, wynik) =>{
      var nowyCache = cache + (x -> wynik)
      cache = nowyCache
      context.parent ! Wynik(x, wynik)
    }
  }
}

case class Wynik(n: Int, fib: BigInt)

class Pracownik(k: Int) extends Actor with ActorLogging {
  def fibonacci(n: Int): BigInt = {
      @annotation.tailrec
      def loop(n: Int, prev: BigInt, cur: BigInt): BigInt =
        if (n <= 0) prev
        else loop(n - 1, cur, prev + cur)
      loop(n, 0, 1)
    }

  def receive: Receive ={
    case Oblicz(x)=>
      context.parent ! Wynik(x, fibonacci(x))
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