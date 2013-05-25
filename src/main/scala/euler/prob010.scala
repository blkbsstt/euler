package euler

object Problem010 extends App {
	println(prime.sieve(2000000).foldLeft(0.big)(_+_))
}