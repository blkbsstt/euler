import com.azavea.math.Numeric
import com.azavea.math.EasyImplicits._
import com.azavea.math.ConvertableFrom
import Predef.{any2stringadd => _, _}
import collection.mutable
import collection.immutable

package object euler {

	implicit class NumericImplicits[T: Numeric](n: T) {
		val even = n % 2 === 0
		val odd = !even

		def factorial: T = if (n <= 1) 1 else n * ((n - 1) factorial)
		def divides[U: Numeric](m: U) = m % n === 0
		def *|[U: Numeric](m: U) = timesDivisible(m,n)

		def palindromic: Boolean = n === n.reversed
		def reversed: T = BigDecimal(n.toString.reverse)
		def big: BigInt = numeric.toType[BigInt](n)
		def pow[U: ConvertableFrom](m: U) = numeric.pow(n, m)
		def **[U: ConvertableFrom](m: U) = numeric.pow(n, m)

		def digits: Seq[Int] = n.toString.map(asDigit)

		def congruent[U: Numeric](m: U) = new Congruence(n,m)
		def ≡[U: Numeric](m: U) = n congruent m
		
		val divisorMemo = collection.mutable.HashMap.empty[T, Seq[T]]
		def divisors: Seq[T] = {
			if (!divisorMemo.contains(n)) {
				val factors = prime.factors(n).flatMap{ case (x:T,y:Int) => List.fill(y)(x) }
				val combs = factors.indices.flatMap(i => factors.combinations(i + 1).map(_.reduce((x:T,y:T) => x * y)))
				divisorMemo(n) = numeric.one +: combs
			} 
			divisorMemo(n)
		}

		def divisorCount = prime.factors(n).map{ case (x, y) => y + 1 }.product

		def \ [U: Numeric](m: U) = Rational(n, implicitly[Numeric[T]].fromType(m))

        implicit def fromConvertable[U: ConvertableFrom](m: U): T = numeric.fromType(m)

        /*
          implicit def fromByte(a:Byte): T = numeric.fromType(a)
          implicit def fromShort(a:Short): T = numeric.fromType(a)
          implicit def fromInt(a:Int): T = numeric.fromType(a)
          implicit def fromLong(a:Long): T = numeric.fromType(a)
          implicit def fromFloat(a:Float): T = numeric.fromType(a)
          implicit def fromDouble(a:Double): T = numeric.fromType(a)
          implicit def fromBigInt(a:BigInt): T = numeric.fromType(a)
          implicit def fromBigDecimal(a:BigDecimal): T = numeric.fromType(a)
        */
	}

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

	def gcd[T: Numeric](a: T, b: T): T = if (b === 0) a else gcd(b, a % b)

	class Rational[T: Numeric](num: T, denom: T) {
		private val g = gcd(num,denom)
		val n = num/g
		val d = denom/g

		def unary_- = -n \ d
		def reciprocal = Rational(d, n)

		def *[U: Numeric](o: Rational[U]) = Rational(n * o.n, d * o.d)
		def +[U: Numeric](o: Rational[U]) = Rational(n * o.d + o.n * d, d * o.d)
		def -[U: Numeric](o: Rational[U]) = this + (-o)
		def /[U: Numeric](o: Rational[U]) = this * (o.reciprocal)

		def *[U: Numeric](n: U): Rational[T] = this * Rational(n)
		def +[U: Numeric](n: U): Rational[T] = this + Rational(n)
		def -[U: Numeric](n: U): Rational[T] = this - Rational(n)
		def /[U: Numeric](n: U): Rational[T] = this / Rational(n)

		override def toString = "" + n + "\\" + d
		def == [U: Numeric](o: Rational[U]) = if (eq(o)) true else if (null == o) false else equals(o)
		def equals [U: Numeric](o: Rational[U]) = (n === o.n) && (d === o.d)
	}


	object Rational {
		def apply[T: Numeric](n: T, d: T) = new Rational(n, d)
		def apply[T: Numeric](n: T) = new Rational(n, numeric.one)
		def unapply[T: Numeric](r: Rational[T]) = Some(r.n, r.d)
	}

	object \ {
		def unapply[T: Numeric](r: Rational[T]) = Some(r.n, r.d)
	}

	implicit def RationalToDouble[T: Numeric](r: Rational[T]): Double = r.n.toDouble / r.d.toDouble

	object prime{

		object sieve {
			def apply(n: Int) = atkin(n)

			def eratosthenes(limit: Int): Seq[Int] = {
				assume(limit > 1)
				val a = Array.fill(limit + 1)(true)
				for(i <- 2 to math.sqrt(limit).toInt; if a(i); j <- i*i to limit by i) a(j) = false 
				for(i <- 2 to limit; if a(i)) yield i
			}

			def atkin(limit: Int): Seq[Int] = {
				if(limit.abs <= 3) return List(limit.abs)
				val a = Array.ofDim[Boolean](limit + 1)
				def toggle(n: Int) = {a(n) = !a(n)}

				val sqrt = math.sqrt(limit).toInt
				for(x <- 1 to sqrt; y <- 1 to sqrt) {
					val (x2, y2) = (x ** 2, y ** 2)
					var n = 4 * x2 + y2
					if (n <= limit && ((n ≡ 1 mod 12) || (n ≡ 5 mod 12))) toggle(n)
					n -= x2
					if (n <= limit && (n ≡ 7 mod 12)) toggle(n)
					n -= 2 * y2
					if (x > y && n <= limit && (n ≡ 11 mod 12)) toggle(n)
				}

				for(n <- 5 to sqrt; if a(n); n2 = n*n; m <- n2 to limit by n2) a(m) = false

				List(2, 3) ++ a.indices.filter(a)
			}
		}

		//is all crappy right now. distinct factors doesnt actually give distinct factors, and is probably really slow. argh.

		def distinctFactors[T: Numeric](n: T): Seq[T] = {
			trialDivide(n)
		}

		def factors[T: Numeric](n: T): Seq[(T, Int)] = {
			trialDivide(n).map(x => (x, x *| n))
		}

		private def trialDivide[T: Numeric](n: T): Seq[T] = {
			val list = mutable.ListBuffer.empty[T]
			var m = n
			var i = numeric.fromType(2)
			while(m > 1) {
				if(i divides m) list += i;
				while(i divides m) m /= i
				i += 1
			}
			return list
		}
	}
 
}
