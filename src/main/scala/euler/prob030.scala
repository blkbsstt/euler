package euler

object Problem030 extends App {
	println((2 to 300000).filter(i => i.digits.map(j => j ** 5).sum == i).sum)
}