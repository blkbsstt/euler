package euler
object Problem001 extends App {
    println(Iterator.from(1).filter(x => x % 3 == 0 || x % 5 == 0).takeWhile(_ < 1000).sum)
}
