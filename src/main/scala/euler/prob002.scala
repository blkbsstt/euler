package euler
object Problem002 extends App {
    def fibHelper(a: Int, b: Int): Stream[Int] = b #:: fibHelper(b, a+b)
    val fibStream = 1 #:: fibHelper(1,1)
    println(fibStream.filter(_ % 2 == 0).takeWhile(_ < 4000000).sum)
}
