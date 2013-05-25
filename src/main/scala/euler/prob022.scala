package euler

object Problem022 extends App {
	println(io.Source.fromFile("names.txt").mkString.replaceAllLiterally("\"", "").split(",").toList.sorted.map(_.map(_ - 'A' + 1).sum).zipWithIndex.map(x => x._1 * (x._2 + 1)).sum)
}