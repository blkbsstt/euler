package euler

object Problem009 extends App {
	val opts = for(i <- 1 to 998; j <- 1 to 999 - i; k = 1000 - i - j; if (i < j && j < k)) yield (i,j,k)
	val triplet = opts.find{case (i,j,k) => i*i + j*j == k*k}
	val Some(solution) = triplet.map{case (i,j,k) => i * j * k}
	println(solution)
}