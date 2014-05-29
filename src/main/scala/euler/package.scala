import spire.math._
import spire.math.prime.Factors
import spire.math.ConvertableFrom._
import spire.implicits._
import spire.algebra._
import collection.mutable
import collection.immutable

package object euler {

    implicit class IntegralImplicits[T: Integral](i: T) {
        val even = (i % 2) == 0
        val odd = !even
        def divides(j: T) = i % j == 0
        val big = implicitly[Integral[T]].toBigInt(i)
        val natural = Natural(i.big)
        val digits = i.natural.toList.map(_.toInt)
        val palindromic = i.natural.reversed == i.natural
        def factorial: T = if (i <= 1) implicitly[Integral[T]].fromInt(1) else i * ((i - 1) factorial)
        def **(k: Int) = i.pow(k)
        def congruent(j: T) = new Congruence(i,j)
        def ≡(j: T) = i congruent j

        def factors = Factors(i.big).toMap.map{ case (x, y) => (ConvertableFromSafeLong.toType[T](x), y) }
        def uniqueFactors = factors.keySet

        val divisorMemo = collection.mutable.HashMap.empty[T, Seq[T]]
        def divisors: Seq[T] = {
            if (!divisorMemo.contains(i)) {
                val factors: Seq[T]  = i.factors.flatMap(
                    (p:(T, Int)) => List.fill(p._2)(p._1)
                ).toSeq
                val combs = factors.indices.flatMap(
                    j => factors.combinations(j + 1).map(_.reduce((x:T, y:T) => x * y))
                )
                divisorMemo(i) = implicitly[Integral[T]].fromInt(1) +: combs
            }
            divisorMemo(i)
        }

        def divisorCount = factors.values.sum
    }

    def asDigit(c: Char) = c.asDigit

    class Congruence[T: Integral](a: T, b: T) {
        def mod(c: T): Boolean = c divides (a - b)
    }

    object prime {
        def sieve(n: Int) = primes.takeWhile(_ <= n)
    }
    // Should be using Natural, but it doesn't have isPrime. Sad face.
    def naturals = naturalsFrom(SafeLong.one)
    def naturalsFrom(n: SafeLong): Stream[SafeLong] = n #:: naturalsFrom(n + 1)
    def primes = naturals.filter(_.isPrime)

    /*
    class NaturalImplicits(n: Natural) {
        val length = n.getDigitLength
        val even = n.isEven
        val odd = n.isOdd
        def divides(m: Natural) = n % m == 0
        val palindromic = n.reversed == n
    }
    //implicit def NaturalisNaturalImplicit(n: Natural) = new NaturalImplicits(n)

    //def naturalsFrom(n: Natural): Stream[Natural] = n #:: naturalsFrom(n + UInt(1))
    //def naturals = naturalsFrom(Natural(0))

    implicit class NumericImplicits[T: Number](n: T) {
        val even = n % 2 === 0
        val odd = !even

        def factorial: T = if (n <= 1) 1 else n * ((n - 1) factorial)
        def divides[U: Number](m: U) = m % n === 0
        def *|[U: Number](m: U) = timesDivisible(m,n)

        def palindromic: Boolean = n === n.reversed
        def reversed: T = BigDecimal(n.toString.reverse)
        def big: BigInt = numeric.toType[BigInt](n)
        //def pow[U: ConvertableFrom](m: U) = numeric.pow(n, m)
        //def **[U: ConvertableFrom](m: U) = numeric.pow(n, m)

        def digits: Seq[Int] = n.toString.map(asDigit)

        def congruent[U: Number](m: U) = new Congruence(n,m)
        def ≡[U: Number](m: U) = n congruent m

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

        def \ [U: Number](m: U) = Rational(n, implicitly[Number[T]].fromType(m))

        //implicit def fromConvertable[U: ConvertableFrom](m: U): T = numeric.fromType(m)
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
            def apply(n: Int) = eratosthenes(n)

            def eratosthenes(limit: Int) = {
                assume(limit > 1)
                val prime = Array.fill(limit + 1)(true)
                prime(0) = false
                prime(1) = false
                /*
                Stream.from(2).takeWhile(i => i * i <= limit).filter(prime).foreach { i =>
                  (i * i to limit by i).foreach(prime(_) = false)
                }
                */
                for(i <- 2 to math.sqrt(limit).toInt; if prime(i); j <- i*i to limit by i) prime(j) = false
                prime.indices.filter(prime)
            }

            def integralMethod(limit: Int) = {
                Stream.from(2).filter(_.isPrime)
            }

            //this is crap, so slow
            /*     wow so slow
             *
             * much crap
             *                    how inefficient
             *
             *         long wait for primes
             */
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

        def distinctFactors[T: Integral](n: T): Seq[T] = {
            trialDivide(n)
        }

        def factors[T: Integral](n: T): Seq[(T, Int)] = {
            trialDivide(n).map(x => (x, timesDivisible(x, n)))
        }

        private def timesDivisible[T: Integral](a: T, b: T): Int = {
            def helper(c: T, n: Int): Int = if(b divides c) helper(c/b, n+1) else n
            helper(a, 0)
        }

        private def trialDivide[T: Integral](n: T): Seq[T] = {
            val list = mutable.ListBuffer.empty[T]
            var m = n
            var i = implicitly[Integral[T]].fromInt(2)
            while(m > 1) {
                if(i divides m) list += i;
                while(i divides m) m /= i
                i += 1
            }
            return list
        }
    }
    */
}
