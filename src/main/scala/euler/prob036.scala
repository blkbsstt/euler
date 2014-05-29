package euler

object Problem036 extends App {
    println((1 to 999999).filter(i => i.palindromic && BigInt(i.toBinaryString).palindromic).sum)
}
