package euler

import java.util.Scanner
import collection.mutable
import collection.JavaConversions._
import annotation.tailrec

object RollerCoaster extends App {
	val s = new Scanner(System.in)

	case class State(dizzy: Int, fun: Int)

	def streamMerge(a: List[State], b: List[State]): Stream[State] = (a,b) match {
		case (Nil, Nil) => Stream.empty
		case (_, Nil) => a.toStream
		case (Nil, _) => b.toStream
		case (x :: xs, y :: ys) => 
			if (x.dizzy > y.dizzy) x #:: streamMerge(xs, b)
			else y #:: streamMerge(a, ys)
	}

	def filter(l: List[State]): List[State] = {
		if(l.size < 2) l
		else {
			val b = mutable.Buffer[State]() ++ l
			val i = b.listIterator
			@tailrec def loop {
				val a = i.next
				if(i.isEmpty) return
				val b = i.next
				i.previous
				if(a.fun <= b.fun) {
					i.previous; i.remove
					if(i.hasPrevious) i.previous
				}
				loop
			}
			loop
			b.toList
		}
	}

	@tailrec def run {
		val Seq(n, k, l) = Stream.continually(s.nextInt).take(3)
		if (n != 0 || k != 0 || l != 0) {
			val Seq(f, d) = (for (i <- 0 until n) yield Seq(s.nextInt, s.nextInt)).transpose

			@tailrec def solve(i: Int, seq: Seq[State]): Seq[State] = {
				if(i >= n) seq
				else {
					val open = for(s <- seq; z = s.dizzy + d(i) if z <= l) yield State(z, s.fun + f(i))
					val closed = for(s <- seq; z = s.dizzy - k) yield State(if (z > 0) z else 0, s.fun)
					var merged = streamMerge(open.toList, closed.toList).toList
					solve(i + 1, filter(merged))
				}
			}

			val State(dizzy, fun) = solve(0, List(State(0,0))).maxBy(_.fun)
			System.out.println(fun)
			run
		}
	}

	run
}