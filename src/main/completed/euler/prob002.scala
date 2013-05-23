package euler
object Problem002 extends App {
    def fib(a: Int, b: Int): Stream[Int] = b #:: fib(b, a + b)
    println((1 #:: fib(1, 1)) filter(_ % 2 == 0) takeWhile(_ < 4000000) sum)
}
