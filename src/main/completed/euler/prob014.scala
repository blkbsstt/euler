package euler

object Problem014 extends App {
	val memo = collection.mutable.HashMap(1l -> 1l)

	def collatz(n: Long): Long = {
		def helper(l: List[Long]): Long = {
			if(memo.contains(l.head)) memoList(l.tail, memo(l.head) + 1)
			else {
				val m = if(l.head even) l.head/2 else 3*l.head + 1 
				helper(m :: l)
			} 
		}

		def memoList(l: List[Long], i: Long): Long = l match {
			case x :: xs => {
				memo(x) = i
				memoList(xs, i + 1)
			}
			case Nil => i - 1
		}

		helper(List(n))
	}

	println((1l until 1000000l).maxBy(collatz))
}