package euler

object Problem032 extends App {
	val l = List(1,2,3,4,5,6,7,8,9).sorted
	println((for(i <- 1 to 98; j <- 123 to 9876; p = i * j; if (i.digits ++ j.digits ++ p.digits).sorted == l) yield p).distinct.sum)
}