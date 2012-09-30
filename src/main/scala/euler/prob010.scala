package euler

object Problem010 extends App {
	println(prime.sieve.atkin(2000000).foldLeft(BigInt(0))(_+_))
}