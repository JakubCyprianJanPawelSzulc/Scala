package lab07

case class Ocena(imię: String, nazwisko: String, wdzięk: Int, spryt: Int) {
  require(
    imię.trim() != "" &&
    nazwisko.trim() != "" &&
    (0 to 20).contains(wdzięk) &&
    (0 to 20).contains(spryt)
  )
}

case class Wynik(
  miejsce: Int,
  imię: String,
  nazwisko: String,
  średniWdzięk: Double,
  średniSpryt: Double,
  suma: Double
) {
  require(
    miejsce >= 0 &&
    imię.trim() != "" &&
    nazwisko.trim() != "" &&
    średniWdzięk >= 0 && średniWdzięk <= 20 &&
    średniSpryt >= 0 && średniSpryt <= 20 &&
    suma == średniWdzięk + średniSpryt
  )
}

def obliczWyniki(oceny: Seq[Ocena]): Any = {
  val grupyOcen = oceny.groupBy(o => (o.imię, o.nazwisko))
  val wyniki = grupyOcen.map { case ((imie, nazwisko), listaOcen) =>
    val sredniaWdziek = listaOcen.map(_.wdzięk).sum.toDouble / listaOcen.size
    val sredniaSpryt = listaOcen.map(_.spryt).sum.toDouble / listaOcen.size
    (imie, nazwisko, sredniaWdziek, sredniaSpryt)
  }
  val wynikiSuma = wyniki.map { case (imie, nazwisko, sredniaWdziek, sredniaSpryt) =>
    (imie, nazwisko, sredniaWdziek, sredniaSpryt, sredniaWdziek + sredniaSpryt)
  }
  
  val wynikiSorted = wynikiSuma.toList.sortBy(-_._5)(Ordering.Double.IeeeOrdering)
  val wynikiIndexed = wynikiSorted.zipWithIndex.map { case ((imie, nazwisko, sredniaWdziek, sredniaSpryt, suma), index) =>
    Wynik(index + 1, imie, nazwisko, sredniaWdziek, sredniaSpryt, suma)
  }
  val wynikiIndexedExaqua = wynikiIndexed.map(
    w => if (w.suma == wynikiIndexed(w.miejsce).suma) w.copy(miejsce = wynikiIndexed(w.miejsce).miejsce) else w
  )
  wynikiIndexedExaqua
  
}

@main def zadanie_03: Any = {
  val wyniki= Seq(Ocena("Tadek","Cracovia",7,7),Ocena("Jacek", "Jaworek", 1, 20), Ocena("Jacek", "Jaworek", 1, 20), Ocena("Tomasz", "Dolinski", 6, 8))
  println(obliczWyniki(wyniki))
}
