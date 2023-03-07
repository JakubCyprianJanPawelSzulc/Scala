package lab03

def hipoteza(liczba: Int): Unit = {
    @annotation.tailrec
    def pomocnicza(a: Int, b: Int): Unit = {
        if (a>b) println(s"$b + $a")
        else if (czyPierwsza(a, a-1) && czyPierwsza(b, b-1) && a+b==liczba) println(s"$a + $b")
        else pomocnicza(a+1, b-1)

    }
    @annotation.tailrec
    def czyPierwsza(k: Int, dzielnik: Int): Boolean = {
        if (dzielnik == 1) true
        else if (k % dzielnik == 0) false
        else czyPierwsza(k, dzielnik - 1)
    }
    
    if (liczba<4) println("Liczba nie może być przedstawiona jako iloczyn liczb pierwszych")
    else pomocnicza(2, liczba-2)  
}

@main def zad_02(liczba: Int): Unit = {
    require(liczba>2 && liczba%2==0)
    //Zdefiniuj funkcję hipoteza, która jako argument pobiera 
    //parzystą liczbę naturalną większą od 2 oraz 
    //sprawdza czy jest ona sumą dwóch liczb pierwszych. 
    //Jeżeli tak, to funkcja hipoteza powinna wypisać je na konsoli. 
    //W przeciwnym wypadku na konsoli powinien pojawić się komunikat mówiący, że 
    //liczb takich nie udało sie znaleźć. 
    hipoteza(liczba)
}