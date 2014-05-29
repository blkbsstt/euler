package euler

object Problem040 extends App {
    val champ = {
        def helper(s: String, n: Int): Stream[Int] = {
            if (s isEmpty) helper(n.toString, n + 1)
            else s.head.asDigit #:: helper(s.tail, n)
        }
        helper("0", 1)
    }

	val items = List(1, 10, 100, 1000, 10000, 100000, 1000000)
    println(items.map(champ).product)
}
