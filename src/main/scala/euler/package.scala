import com.azavea.math.Numeric
import com.azavea.math.EasyImplicits._
import com.azavea.math.ConvertableFrom
import Predef.{any2stringadd => _, _}

package object euler {
	def from[T: Numeric](n: T, s: T => T): Stream[T] = n #:: from(s(n), s)
	def from[T: Numeric, U: Numeric](n: T, s: U): Stream[T] = from(n, (x: T) => x + s)
	def from[T: Numeric](n: T): Stream[T] = from(n, 1)


	class NumericImplicits[T: Numeric](n: T) {
		val even = n % 2 === 0
		val odd = !even

		def factorial: T = if (n <= 1) 1 else n * ((n - 1) factorial)
		def divides[U: Numeric](m: U) = m % n === 0
		def *|[U: Numeric](m: U) = timesDivisible(m,n)
		def **(m: T): T = numeric.pow(n, m)

		def palindromic: Boolean = n === n.reversed
		def reversed: T = BigInt(n.toString.reverse)
		def big: BigInt = numeric.toType[BigInt](n)
		def pow[U: ConvertableFrom](m: U) = numeric.pow(n, numeric.fromType(m))
		def **[U: ConvertableFrom](m: U) = numeric.pow(n, numeric.fromType(m))

		def digits: Seq[Int] = n.toString.map(asDigit)

		def congruent[U: Numeric](m: U) = new Congruence(n,m)
		def ≡[U: Numeric](m: U) = n congruent m


	  implicit def fromByte(a:Byte): T = numeric.fromType(n)
	  implicit def fromShort(a:Short): T = numeric.fromType(n)
	  implicit def fromInt(a:Int): T = numeric.fromType(n)
	  implicit def fromLong(a:Long): T = numeric.fromType(n)
	  implicit def fromFloat(a:Float): T = numeric.fromType(n)
	  implicit def fromDouble(a:Double): T = numeric.fromType(n)
	  implicit def fromBigInt(a:BigInt): T = numeric.fromType(n)
	  implicit def fromBigDecimal(a:BigDecimal): T = numeric.fromType(n)
	}

	implicit def NumericImplicits[T: Numeric](n: T): NumericImplicits[T] = new NumericImplicits[T](n)
		
	def even[T: Numeric](n: T) = n even
	def odd[T: Numeric](n: T) = n odd
	def reverse[T: Numeric](n: T) = n reversed

	def asDigit(c: Char) = c.asDigit

	class Congruence[T: Numeric, U: Numeric](a: T, b: U) {
		def mod[V: Numeric](c: V): Boolean = c divides (a - b)
	}

	def timesDivisible[T: Numeric, U: Numeric](a: T, b: U): Int = {
		def helper(c: T, n: Int): Int = if(b divides c) helper(c/b, n+1) else n
		helper(a, 0)
	}

	object prime{

		object sieve {
			def apply(n: Int) = atkin(n)
			def atkin(max: Int): Seq[Int] = {
				if(math.abs(max) <= 3) return List(math.abs(max))
				val a = Array.ofDim[Boolean](max/2)
				def toggle(n: Int) = {a((n-1)/2) = !a((n-1)/2)}

				val lim = math.sqrt(max).toInt
				for(x <- 1 to lim; y <- 1 to lim) {
					val (x2, y2) = (x ** 2, y ** 2)
					var n = 4 * x2 + y2
					if (n <= max && ((n ≡ 1 mod 12) || (n ≡ 5 mod 12))) toggle(n)
					n -= x2
					if (n <= max && (n ≡ 7 mod 12)) toggle(n)
					n -= 2 * y2
					if (x > y && n <= max && (n ≡ 11 mod 12)) toggle(n)
				}

				for(n <- 5 to lim; if a(n); n2 = n*n; m <- n2 to max by n2) a((m-1)/2) = false

				List(2, 3) ++ a.indices.filter(a).map(_ * 2 + 1)
			}
		}

		//is all crappy right now. distinct factors doesnt actually give distinct factors, and is probably really slow. argh.

		def distinctFactors(n: Long): Seq[Int] = {
			val lim = math.sqrt(n).toInt
			prime.sieve(lim).find(_ divides n) match {
				case None => List(n.toInt)
				case Some(1) => List(n.toInt)
				case Some(x) => List(x) ++ distinctFactors(n/x)
			}
		}

		def factors(n: Long): Seq[(Int, Int)] = {
			distinctFactors(n).map(x => (x, x *| n))
		}
	}
 
}
