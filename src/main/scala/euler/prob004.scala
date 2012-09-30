package euler

object Problem004 extends App {
	println((for(i <- 100 to 999; j <- 100 to 999; p = i * j; if p palindromic) yield p).max)
}