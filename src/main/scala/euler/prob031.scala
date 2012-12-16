package euler

object Problem031 extends App {
	val coins = List(200, 100, 50, 20, 10, 5, 2, 1)
	val memo = collection.mutable.HashMap.empty[(Int, Int), Int]
	def change(n: Int, i: Int): Int = {
		memo.getOrElseUpdate((n, i),
			if(n == 0) 1
			else if(n <= 0 || i >= coins.size) 0
			else change(n - coins(i), i) + change(n, i + 1)
		)
	}

	println(change(200, 0))
}