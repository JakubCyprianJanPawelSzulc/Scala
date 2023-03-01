package lab01

def jestPierwsza(n: Int): Boolean = {
  require(n >= 2)

  def pomocnicza(a: Int, b: Int): Boolean = {
    if (b == 1) true
    else if (a % b == 0) false
    else pomocnicza(a, b - 1)
  }

  pomocnicza(n, n - 1)
}

@main def zad_01: Unit = {
  print("Podaj liczbę naturalną: ")
  val liczba = io.StdIn.readInt()
  val wynik = if jestPierwsza(liczba) then "" else " nie"
  println(s"Liczba $liczba$wynik jest liczbą pierwszą")
}
