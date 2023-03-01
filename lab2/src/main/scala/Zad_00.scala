package lab01

@main def przykłady: Unit = {

  //-------------------------------------------------------------------
  // Wartości całkowite
  //-------------------------------------------------------------------
  val anInt = 3
  val anIntWithExplicitType: Int = 3

  //-------------------------------------------------------------------
  // Wartości zmiennopozycyjne:
  //-------------------------------------------------------------------
  val aDouble = 4.0
  val aDoubleWithExplicitType: Double = 4.0

  //-------------------------------------------------------------------
  // Znaki:
  //-------------------------------------------------------------------
  val aCharacter = 'A'
  val aCharacterWithExplicitType: Char = 'A'

  //-------------------------------------------------------------------
  // Łańcuchy znaków (napisy):
  //-------------------------------------------------------------------
  val aString = "Ala ma kota"
  val aStringWithExplicitType: String = "Ala ma kota"
  val interpolatedString = s"${aCharacter}la ma kota i psa"
  println(s"Napis „$aString” ma długość ${aString.length}.")

  // przydatny mechanizm „wielokrotnego powtarzania napisu”:
  val eightStars = "*" * 8
  println(s"Krótki „szlaczek”: $eightStars")

  // Inny przydatny mechanizm – „rozbijanie” napisu na tablicę napisów
  val splitString = "xy--xy--xy--xy".split("--")
  // wynikiem będzie tablica: Array("xy", "xy", "xy", "xy")

  //-------------------------------------------------------------------
  // Tablice:
  //-------------------------------------------------------------------
  val anArray = Array(1, 2, 3, 5)
  // z elemetów tablicy tworzymy napis używając „separatora”:
  val elements = anArray.mkString(",")
  // inny przykład – oprócz separatora dodajemy „prefiks” i „sufiks”
  val elements2 = anArray.mkString("Array(", ",", ")")
  // jeszcze inny przykład
  val elements3 = anArray.mkString("łańcuszek: ", "->", ".")

  println(s"Pierwszy element w Array($elements) to ${anArray(0)}")
  val anArrayWithExplicitType: Array[Int] = Array(1, 2, 3, 5)

  // liczba elementów w tablicy:
  val numOfElemsInSpiltString = splitString.length

  //-------------------------------------------------------------------
  // „Krotki” (pary, trójki, czwórki, piątki, …):
  //-------------------------------------------------------------------
  val aPair = (aString, aDouble)
  val aPairWithExplicitType: (Int, String) = (anInt, aString)

  val aTriple = (aString, aDouble, aString)
  val aTripleWithExplicitType: (Int, Int, Int) = (anInt, anInt, anInt)

  //-------------------------------------------------------------------
  // Zakresy i „wyliczenia for”:
  //-------------------------------------------------------------------
  val aRange1 = 1 to 5
  val aaRange1WithExplicitType: Range = 1 to 5
  println("Zakres – przykład „1 to 5”")
  // użyjemy „wyliczenia for” (uwaga – nie jest to „pętla”)
  for { n <- aRange1 } println(n)
  // w osobnych linijkach pojawią się liczby od 1 do 5

  val aRange2 = 1 until 5
  val aRange2WithExplicitType: Range = 1 until 5
  println("Zakres – przykład „1 until 5”")
  for { n <- aRange2 } println(n)
  // w osobnych linijkach pojawią się liczby od 1 do 4

  // wyliczenie może też użyć „yield” żeby zwrócić wartość jako wynik:
  val arrOfStrings = for { el <- Array(1,2,3,4,5) } yield "#" * el
  // powyżej, „generatorem” wyliczenia jest nie zakres a tablica (!)

  // oczywiście nie musimy wszystkiego pisać „w jednej linii”:
  val arrOfStringsMultiline = for {
    el <- Array(1,2,3,4,5)
  } yield "#" * el

  // elementy zwracane przez generator możemy odfiltrowywać
  val arrOfStringsFilter = for {
    i <- Array(1,-2,3,-4,5) if i>=0
  } yield "#" * i

  //-------------------------------------------------------------------
  // Funkcje (definiowane jako „metody”):
  //-------------------------------------------------------------------
  def inc1(x: Int): Int = x + 1
  def inc2(x: Int): Int = {
    println("jeśli „ciało” funkcji jest bardziej rozbudowane,")
    val str = "to stosujemy notację „blokową”, stosując nawiasy „{…}”"
    println(str)
    println(s"Ta funkcja większa wartość argumentu $x o 2")
    x + 2
  }

  //-------------------------------------------------------------------
  // Funkcje anonimowe (definiowane jak „zwykłe” wartości):
  //-------------------------------------------------------------------
  val inc1Anonymous = (x: Int) => x + 1
  val inc2Anonymous: Int => Int = x => x + 2
  val inc3Anonymous = (x: Int) => {
    println("jeśli „ciało” funkcji jest bardziej rozbudowane,")
    val str = "to stosujemy notację „blokową”, stosując nawiasy „{…}”"
    println(str)
    println(s"Ta funkcja większa wartość argumentu $x o 3")
    x + 3
  }

}
