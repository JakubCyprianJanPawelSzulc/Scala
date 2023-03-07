package lab03

def ciagGeometryczny(n: Int, iloraz: Double, poczatek: Double): Double = {
    @annotation.tailrec
    def pomocnicza(wynik: Double, iloraz: Double, liczbakoncowa: Int): Double = {
        if (n == liczbakoncowa) wynik
        else pomocnicza(wynik * iloraz, iloraz, liczbakoncowa + 1)
    }
    pomocnicza(poczatek, iloraz, 1)
}
@main def zad_01(n: Int, iloraz: Double, poczatek: Double): Unit = {
    require(n >= 0)
    val wynik = ciagGeometryczny(n, iloraz, poczatek)
    println(s"a_$n dla ciagu a_n=$poczatek*($iloraz^n) wynosi: $wynik")
}
