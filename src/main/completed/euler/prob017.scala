package euler

object Problem017 extends App {
	def toWord(n: Int): String = n match {
		case 0 => ""
		case 1 => "one"
		case 2 => "two"
		case 3 => "three"
		case 4 => "four"
		case 5 => "five"
		case 6 => "six"
		case 7 => "seven"
		case 8 => "eight"
		case 9 => "nine"
		case 10 => "ten"
		case 11 => "eleven"
		case 12 => "twelve"
		case 13 => "thirteen"
		case 15 => "fifteen"
		case 18 => "eighteen"
		case i if i < 20 => toWord(i%10) + "teen"
		case 20 => "twenty"
		case 30 => "thirty"
		case 40 => "forty"
		case 50 => "fifty"
		case 80 => "eighty"
		case i if i < 100 && (10 divides i) => toWord(i/10) + "ty"
		case i if i < 100 => toWord((i/10)*10) + toWord(i%10)
		case i if i < 1000 => {
			val rest = toWord(i%100)
			toWord(i/100) + "hundred" + (if(rest.nonEmpty) "and" + toWord(i%100) else "")
		}
		case i if i < 1000000 => toWord(i/1000) + "thousand" + toWord(i%1000)
	}

	def numToWord(n: Int) = n match {
		case 0 => "zero"
		case x => toWord(x)
	}

	println((1 to 1000).map(i => numToWord(i).size).sum)
}