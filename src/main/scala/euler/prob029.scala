package euler

object Problem029 extends App {
	println((for(a <- 2 to 100; b <- 2 to 100) yield a.big ** b).distinct.size)
}