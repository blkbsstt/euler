package euler
object Problem001 extends App {
    println(from(1).filter(x => (3 divides x) || (5 divides x)).takeWhile(_ < 1000).sum)
}