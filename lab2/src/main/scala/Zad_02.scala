package lab01

/*
  Funkcja „obramuj” „zdefiniowana” poniżej wykorzystuje dwa parametry
    - „napis” potencjalnie może mieć kilka linijek (patrz przykład)
    - „znak”, z którego należy zbudować ramkę
*/
def obramuj(napis: String, znak: Char): String = {
  val wiersze = napis.split("\n")
  val najdluzszy = wiersze.maxBy(_.length)
  val ramka = znak.toString * (najdluzszy.length + 4)

  val wierszeObramowane = wiersze.map { wiersz =>
    val brakujace = najdluzszy.length - wiersz.length
    val spacje = " " * brakujace
    s"$znak $wiersz$spacje $znak"
  }

  (ramka :: wierszeObramowane.toList ::: List(ramka)).mkString("\n")
}

@main def zad_02: Unit = {
  val wynik = obramuj("ala\nma\nkota", '*')
  println(wynik)
  /*
    Efektem powino być coś podobnego do:

    ********
    * ala  *
    * ma   *
    * kota *
    ********

  */
}
