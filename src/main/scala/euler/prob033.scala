package euler
import spire.math.Rational

object Problem033 extends App {
    println( (for (
         i <- 1 to 9;
         j <- 0 to 9;
         k <- 1 to 9;
         l <- 0 to 9;
         if i * 10 + j < k * 10 + l;
         if !(j == 0 && l == 0);
         v = Rational(i * 10 + j, k * 10 + l);
         if (i == k && Rational(j, l) == v) || 
            (i == l && Rational(j, k) == v) ||
            (j == k && Rational(i, l) == v) ||
            (j == l && Rational(i, k) == v)
        ) yield v).reduce(_ * _).denominator )
}
