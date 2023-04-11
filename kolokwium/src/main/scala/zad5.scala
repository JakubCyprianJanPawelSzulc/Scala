/*
    Korzystając wyłącznie z operacji na kolekcjach (w szczególności nie możesz użyć rekurencji
    ani mechanizmów imperatywnych, takich jak zmienne i pętle) zdefiniuj funkcję:
        def findPairs(n: Int): Set[(Int,Int)]
    taką, że dowolnej liczby całkowitej N > 1
        findPairs(N)
    zawiera wszystkie pary postaci (p1, p2), gdzie p1 i p2 są liczbami
    pierwszymi takimi, że p1 + p2 = 2 * N oraz p1 <= p2.
*/

def findPairs(num: Int): Any = {
    def isPrime(n: Int): Boolean = {
        if (n <= 1) false
        else if (n == 2) true
        else if (n % 2 == 0) false
        else (3 to Math.sqrt(n).toInt by 2).forall(n % _ != 0)
    }

    val primes = (1 to num*2-1).filter(isPrime(_)).toSet
    val pairs = primes.foldLeft(Set[(Int, Int)]())((acc, p1) => {
        val p2 = 2 * num - p1
        if (primes.contains(p2)) {
            acc + ((p1, p2))
        } else acc
    })
    pairs

}

@main
def zad5(): Unit = {
   println(findPairs(17))
}