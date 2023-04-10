package lab07

case class Ocena(
  imię: String,
  nazwisko: String,
  wdzięk: Int,
  spryt: Int
) {
  require(
    // upewniamy się, że składowe Oceny są sensowne
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
  // upewniamy się, że składowe Wyniku są „sensowne”
  require(
    miejsce >= 0 &&
    imię.trim() != "" &&
    nazwisko.trim() != "" &&
    średniWdzięk >= 0 && średniWdzięk <= 20 &&
    średniSpryt >= 0 && średniSpryt <= 20 &&
    suma == średniWdzięk + średniSpryt
  )
}

def obliczWyniki(oceny: Seq[Ocena]): Seq[Wynik] ={
    val grouped = oceny.groupBy(ocena=>(ocena.imię,ocena.nazwisko))
    val listaOsobyWyniki = grouped.map{case ((imię, nazwisko), listaOcen)=>{
        val średniWdzięk = listaOcen.map(_.wdzięk).sum.toDouble/listaOcen.size
        val średniSpryt = listaOcen.map(_.spryt).sum.toDouble/listaOcen.size
        (imię,nazwisko,średniWdzięk, średniSpryt, średniSpryt+średniWdzięk)
    }}
    
    val krzys = listaOsobyWyniki.toSeq.sortBy(x=>(-x._5,-x._3,x._2)).zipWithIndex.map{case ((imię,nazwisko,średniWdzięk, średniSpryt, suma),index)=>{
        Wynik(index+1,imię,nazwisko,średniWdzięk, średniSpryt, suma)
    }}

    val jacek = krzys.foldLeft(List[Wynik](), 1)({
      case ((acc, miejsce), curr) =>
        if (acc.isEmpty) (List(curr.copy(miejsce = miejsce)), miejsce + 1)
        else if (curr.suma == acc.head.suma && curr.średniSpryt == acc.head.średniSpryt && curr.średniWdzięk == acc.head.średniWdzięk)  {
        (curr.copy(miejsce = acc.head.miejsce) :: acc, miejsce)}
        else (curr.copy(miejsce = miejsce) :: acc, miejsce + 1)
    })

    jacek(0).reverse
}


@main def zad_03: Unit = {
  val wyniki= Seq(Ocena("Tadek","Cracovia",7,7),Ocena("Jacek", "Jaworek", 3, 20), Ocena("Jacek", "Jaworek", 1, 20), Ocena("Tomasz", "Dolinski", 6, 8), Ocena("Piesek", "Leszek", 2, 20), Ocena("Tytus", "Bomba", 20,20))
  println(obliczWyniki(wyniki))
}