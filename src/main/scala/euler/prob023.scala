package euler
import math.sqrt
object Problem023 extends App {
	val posNeg = Stream.iterate(1)(i => if(i > 0) -i else -i + 1)
	val genPent = posNeg.map(i => (3 * i * i - i)/2)
	val l: Stream[List[Int]] = List(1,1,-1,-1) #:: l
	val coeffs = l.flatten
	val divMemo = collection.mutable.HashMap.empty[Int, Int]
	def recursiveDivisorSum(n: Int): Int = {
		if(!divMemo.contains(n)) {
			def helper(m: Int) = if(m == n) n else recursiveDivisorSum(n - m)
			divMemo(n) = (genPent.takeWhile(_ <= n).map(helper), coeffs).zipped.map(_ * _).sum
		}
		divMemo(n)
	}

	def divisors(n: Int) = (for(i <- 1 to sqrt(n).toInt; if i divides n) yield List(i, n/i)).flatten.distinct
	def divisorSum(n: Int) = divisors(n).sum
	val memo = collection.mutable.HashMap.empty[Int, Boolean]
	def abundant(n: Int) = memo.getOrElseUpdate(n, (recursiveDivisorSum(n) - n) > n)
	println((1 to 28123).filter(i => 
		!((12 to i/2).map(j => (j, i - j)).exists(t => abundant(t._1) && abundant(t._2)))
	).sum)
}