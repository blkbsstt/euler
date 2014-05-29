package euler
import spire.math._
object Problem001 extends App {
    println(Stream.from(1).filter(x => (3 divides x) || (5 divides x)).takeWhile(_ < 1000).sum)
}
