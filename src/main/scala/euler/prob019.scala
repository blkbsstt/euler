package euler

object Problem019 extends App {
	val months = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	val leapMonths = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	def leap(y: Int): Boolean = (400 divides y) || ((4 divides y) && !(100 divides y))
	def advanceYear(n: Int, y: Int): Int = {
		(if (leap(y)) leapMonths else months).foldLeft(n)((x, y) => (x + y) % 7)
	}
	def advanceMonth(n: Int, m: Int, y: Int): Int = {
		((if (leap(y)) leapMonths else months)(m) + n) % 7
	}
	var count = 0
	var day = advanceYear(1,1900)
	for(y <- 1901 to 2000; m <- 0 to 11) {
		if(day == 0) count += 1
		day = advanceMonth(day, m, y)
	}
	println(count)
}