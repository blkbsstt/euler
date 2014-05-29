package euler
import io.Source
import annotation.tailrec

object Problem054 extends App {
    val inputFile = if(args.size > 0) args(0) else "input/poker.txt"
    println(
        Source.fromFile(inputFile).getLines.count { line =>
            val Seq(player1, player2) = line.split(" ").map(Card(_)).grouped(5).map(Hand(_)).toSeq
            player1 beats player2
        }
    )
}

object Card {
    def apply(c: String): Card = {
        val pattern(v, s) = c
        new Card(value(v), suit(s))
    }

    def value(v: String) = v match {
        case "A" => 14
        case "K" => 13
        case "Q" => 12
        case "J" => 11
        case "T" => 10
        case _   => v.toInt
    }

    def suit(s: String) = s match {
        case "S" => 3
        case "H" => 2
        case "C" => 1
        case "D" => 0
    }

    private val pattern = """([2-9]|[TJQKA])([SHCD])""".r
}

class Card(val value: Int, val suit: Int)

object Hand {
    def apply(l: Seq[Card]): Hand = new Hand(l.toList)

    def flush(hand: List[Card]) = hand.map(_.suit).distinct.size == 1
    def straight(hand: List[Card]) = hand.sliding(2).forall{ case x :: y :: _ => x.value - y.value == 1 }

    sealed abstract class CondCheck(f: List[Card] => Boolean) {
        def unapply(hand: List[Card]) = if (f(hand)) Some(hand.head.value) else None
    }

    sealed abstract class OfAKind(n: Int) {
        def unapply(hand: List[Card]) = hand.map(_.value).combinations(n).
            map(_.distinct).filter(_.size == 1).flatten.toList.sorted.reverse match {
                case high :: _ => Some(high)
                case Nil => None
            }
    }

    object StraightFlush extends CondCheck(hand => straight(hand) && flush(hand))
    object Straight      extends CondCheck(straight)
    object Flush         extends CondCheck(flush)
    object HighCard      extends CondCheck(_ => true)

    object FourOfAKind  extends OfAKind(4)
    object ThreeOfAKind extends OfAKind(3)
    object OnePair      extends OfAKind(2)

    def pairInRemaining(hand: List[Card], value: Int) = hand.filter(_.value != value) match {
        case OnePair(_) => true
        case _ => false
    }

    object FullHouse {
        def unapply(hand: List[Card]) = hand match {
            case ThreeOfAKind(value) if pairInRemaining(hand, value) => Some(value)
            case _ => None
        }
    }

    object TwoPair {
        def unapply(hand: List[Card]) = hand match {
            case OnePair(value) if pairInRemaining(hand, value) => Some(value)
            case _ => None
        }
    }

    @tailrec def tieBreak(a: List[Card], b: List[Card]): Boolean =
        (a.head.value > b.head.value) ||
        (a.head.value == b.head.value && tieBreak(a.tail, b.tail))
}

class Hand(c: List[Card]) {
    import Hand._
    val cards = c.sortBy(_.value).reverse
    val (rank, rankValue) = cards match {
        case StraightFlush(high) => (8, high)
        case FourOfAKind(value)  => (7, value)
        case FullHouse(high)     => (6, high)
        case Flush(high)         => (5, high)
        case Straight(high)      => (4, high)
        case ThreeOfAKind(value) => (3, value)
        case TwoPair(value)      => (2, value)
        case OnePair(value)      => (1, value)
        case HighCard(high)      => (0, high)
    }

    def beats (other: Hand): Boolean = {
        rank > other.rank ||
        (rank == other.rank &&
            (rankValue > other.rankValue ||
                (rankValue == other.rankValue &&
                    tieBreak(this.cards, other.cards))))
    }
}
