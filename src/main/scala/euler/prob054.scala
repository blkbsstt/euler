package euler
import io.Source

object Problem054 extends App {
    val inputFile = if(args.size > 0) args(0) else "poker.txt"
    println( 
        (for(line <- Source.fromFile(if(args.size > 0) args(0) else "poker.txt").getLines) yield { 
            val Seq(player1, player2) = line.split(" ").toList.map(Card(_)).grouped(5).map(Hand(_)).toSeq
            println(line)
            println(Seq(Seq(player1.rank, player1.rankValue), Seq(player2.rank, player2.rankValue)))
            if (player1 > player2) {
                println("Player 1 wins") 
                1
            } else {
                println("Player 2 wins")
                0
            }
        }).sum
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

    val pattern = """([2-9]|[TJQKA])([SHCD])""".r
}

class Card(val value: Int, val suit: Int)

object Hand {
    def apply(l: List[Card]): Hand = new Hand(l)

    def flush(hand: List[Card]) = hand.map(_.suit).distinct.size == 1
    def straight(hand: List[Card]) = hand.sliding(2).forall{ case x :: y :: _ => x.value - y.value == 1 }

    def ofAKind(n: Int, hand: List[Card]) = {
        hand.map(_.value).combinations(n).map(_.distinct).filter(_.size == 1).flatten.toList.sorted match {
            case high :: _ => Some(high)
            case _ => None
        }
    }

    object RoyalFlush {
        def unapply(hand: List[Card]) = {
            hand match {
                case StraightFlush(high) if high == 14 => true
                case _ => false
            }
        }
    }

    object StraightFlush {
        def unapply(hand: List[Card]) = if (straight(hand) && flush(hand)) Some(hand.head.value) else None
    }

    object FourOfAKind {
        def unapply(hand: List[Card]) = ofAKind(4, hand)
    }

    object Straight {
        def unapply(hand: List[Card]) = if (straight(hand)) Some(hand.head.value) else None
    }
            
    object Flush {
        def unapply(hand: List[Card]) = if (flush(hand)) Some(hand.head.value) else None
    }

    object ThreeOfAKind {
        def unapply(hand: List[Card]) = ofAKind(3, hand)
    }

    object OnePair {
        def unapply(hand: List[Card]) = ofAKind(2, hand)
    }

    def pairInRemaining(hand: List[Card], value: Int, takeHighest: Boolean) = hand.filter(_.value != value) match {
        case OnePair(other) if takeHighest => Some(Seq(value, other).max)
        case OnePair(_) => Some(value)
        case _ => None
    }

    object FullHouse {
        def unapply(hand: List[Card]) = {
            hand match {
                case ThreeOfAKind(value) => pairInRemaining(hand, value, false)
                case _ => None
            }
        }
    }

    object TwoPair {
        def unapply(hand: List[Card]) = {
            hand match {
                case FourOfAKind(value) => Some(value)
                case ThreeOfAKind(value) => pairInRemaining(hand, value, true)
                case OnePair(value) => pairInRemaining(hand, value, true)
                case _ => None
            }
        }
    }

    object HighCard {
        def unapply(hand: List[Card]) = Some(hand.head.value)
    }

    def tieBreak(a: List[Card], b: List[Card]): Boolean = {
        (a.head.value > b.head.value) || (a.head.value == b.head.value && tieBreak(a.tail, b.tail))
    }
}

class Hand(c: List[Card]) {
    import Hand._
    val cards = c.sortBy(_.value).reverse 
    val (rank, rankValue) = cards match {
        case RoyalFlush()        => (9, 14)
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
    
    def > (other: Hand): Boolean = {
        rank > other.rank || (rank == other.rank && (rankValue > other.rankValue || (rankValue == other.rankValue && tieBreak(this.cards, other.cards))))
    }

}
