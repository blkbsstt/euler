package euler

object Problem021 extends App {
	def d(n: Int) = n.divisors.sum - n
	val result: Int = (2 until 10000).zip((2 until 10000).map(d)).filter{
		case (x,y) => x == d(y) && x != y
	}.map(_._1).sum
	println(result)
}