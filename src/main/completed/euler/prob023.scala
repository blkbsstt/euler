package euler
import math.sqrt
object Problem023 extends App {
	def divisors(n: Int) = (for(i <- 1 to sqrt(n).toInt; if i divides n) yield Seq(i, n/i)).flatten.distinct
	def divisorSum(n: Int) = divisors(n).sum
	val memo = collection.mutable.HashMap.empty[Int, Boolean]
	def abundant(n: Int) = memo.getOrElseUpdate(n, (divisorSum(n) - n) > n)

    (12 to 20161).map(abundant)

	val nonabundantSums = ((1 to 20161).filter(i => 
		!((12 to i/2).toStream.map(j => (j, i - j)).exists(t => abundant(t._1) && abundant(t._2)))
	))

    println(nonabundantSums.sum)
}
