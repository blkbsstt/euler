package euler

object Problem032 extends App {
    val l = (1 to 9).toList.sorted
	println((for(i <- 1 to 98; j <- 123 to 9876; p = i * j; if (i.digits ++ j.digits ++ p.digits).sorted == l) yield p).distinct.sum)
}
