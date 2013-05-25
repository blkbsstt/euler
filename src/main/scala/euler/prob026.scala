package euler

object Problem026 extends App {
	def recurranceLength(n: Int) = {
		val l = collection.mutable.ListBuffer.empty[Int]
		def helper(i: Int): Int = {
			if(i == 0) 0
			else {
				if(l.contains(i)) l.dropWhile(_ != i).size
				else {
					l += i
					helper((i % n) * 10)
				}
			}
		}
		helper(10)
	}

	println((1 until 1000).maxBy(recurranceLength))
}