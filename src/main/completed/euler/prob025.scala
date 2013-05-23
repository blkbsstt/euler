package euler

object Problem025 extends App {
	val fibMemo = collection.mutable.HashMap.empty[Long, BigInt]
	fibMemo(1) = 1
	fibMemo(2) = 1
	def fib(i: Long): BigInt = fibMemo.getOrElseUpdate(i, fib(i - 1) + fib(i - 2))
	println(Stream.from(1).filter(i => fib(i).digits.size >= 1000).head)
}