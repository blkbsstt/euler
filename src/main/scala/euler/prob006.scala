package euler

object Problem006 extends App {
	println((1 to 100).sum ** 2 - (1 to 100).map(x => x * x).sum)
}