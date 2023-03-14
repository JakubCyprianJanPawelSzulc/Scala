package lab04

def ciąg(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, a: Int, b: Int): Int = n match{
    case 0 => a
    case 1 => b
    case _ => loop(n-1, b, a+b)
  }
  loop(n, 2, 1)
}

@main def zadanie_01(liczba: Int): Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „ciąg”.
  require(liczba >= 0)
  val wynik = ciąg(liczba)
  println(s"$wynik")
}
