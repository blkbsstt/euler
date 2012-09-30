package euler
object Problem002 extends App {
    def fibHelper(a: Int, b: Int): Stream[Int] = b #:: fibHelper(b, a+b)
    val fibStream = 1 #:: fibHelper(1,1)
    println(fibStream filter(even) takeWhile(_ < 4000000) sum)
}
