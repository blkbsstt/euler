package euler

object Problem028 extends App {
	def f(i: Int, s: Int): Stream[Int] = Stream.from(i + s, s).take(4) #::: f(i + s*4, s + 2)
	val s = 1 #:: f(1, 2)
	val i = 1001
	println(s.take(2*(i - 1) + 1).sum)
}