package euler
//slow ~ 6s
object Problem024 extends App {
	println((0 to 9).mkString.permutations.toList.sorted.apply(999999))
}
