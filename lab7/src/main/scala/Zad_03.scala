// package lab07

// case class Ocena(
//   imię: String,
//   nazwisko: String,
//   wdzięk: Int,
//   spryt: Int
// ) {
//   require(
//     // upewniamy się, że składowe Oceny są sensowne
//     imię.strip() != "" &&
//     nazwisko.strip() != "" &&
//     (0 to 20).contains(wdzięk) &&
//     (0 to 20).contains(spryt)
//   )
// }

// case class Wynik(
//   miejsce: Int,
//   imię: String,
//   nazwisko: String,
//   średniWdzięk: Double,
//   średniSpryt: Double,
//   suma: Double
// ) {
//   // upewniamy się, że składowe Wyniku są „sensowne”
//   require(
//     miejsce >= 0 &&
//     imię.strip() != "" &&
//     nazwisko.strip() != "" &&
//     średniWdzięk >= 0 && średniWdzięk <= 20 &&
//     średniSpryt >= 0 && średniSpryt <= 20 &&
//     suma == średniWdzięk + średniSpryt
//   )
// }

// @main def zadanie_03: Unit = {
//   // Uwaga! Poniższy przykład to jedynie „inspiracja”
//   case class O(imię: String, wynik: Int)

//   val wyniki = List(O("Ola",3), O("Ala",1), O("Zosia",2))
//   val klasyfikacja = wyniki.sortBy(o => o match {
//     case O(_, w) => w
//   })
//   println(klasyfikacja)
// }
