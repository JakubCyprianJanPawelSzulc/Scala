@main def mainProg: Unit = {
  println(napis)
}
val napis = "mielewczyk.pl"

@main
def prog2(n: Int): Unit = {
  println(s"Dostałem argument $n")
}

@main
def prog3(imie: String, wiek: Int, czy_gra: Boolean): Unit = {
  val granie = if czy_gra then "gra" else "nie gra"
  println(s"$imie ma $wiek lat i $granie w pilke")
}

@main
def prog4(n: Int): Unit = {
  val rand = scala.util.Random()
  val liczba = rand.nextInt(100)
  println(s"wylosowana liczba to $liczba")
  if n < 0 || n > 100 then return println("liczba poza zakresem")
  if n == liczba then println("trafiles")
  else println("nie trafiles")

}
