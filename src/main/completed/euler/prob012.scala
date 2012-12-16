package euler

object Problem012 extends App {
	def triangle = {
		def helper(n: Int, i: Int): Stream[Int] = {
			(n + i) #:: helper(n + i, i + 1)
		}
		helper(0, 1)
	}

	println(triangle.dropWhile(_.divisorCount <= 500).head)
}