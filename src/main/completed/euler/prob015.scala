package euler

object Problem015 extends App {
	val memo = collection.mutable.HashMap.empty[(Long, Long), Long]
	def paths(n: Long, m: Long): Long = {
		memo.getOrElseUpdate((n,m), 
			if(n == 0 || m == 0) 1
			else paths(n - 1, m) + paths(n, m - 1)
		)
	}

	println(paths(20, 20))
}