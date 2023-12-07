package advent2023.day7

enum CardLabel(private val strength: Int):
    def labelStrength(withJoker: Boolean = false) =
        if this == Jack && withJoker then 1 else strength

    case Numbered(val faceValue: Int) extends CardLabel(faceValue)
    case Jack                         extends CardLabel(11)
    case Queen                        extends CardLabel(12)
    case King                         extends CardLabel(13)
    case Ace                          extends CardLabel(14)

enum HandType(val strength: Int):
    case FiveOfAKind  extends HandType(6)
    case FourOfAKind  extends HandType(5)
    case FullHouse    extends HandType(4)
    case ThreeOfAKind extends HandType(3)
    case TwoPair      extends HandType(2)
    case OnePair      extends HandType(1)
    case HighCard     extends HandType(0)

import HandType.*
import CardLabel.*

case class Hand(cards: List[CardLabel], withJoker: Boolean = false):
    def strength =
        if !withJoker then cards.map(_.labelStrength(false)).sum
        else
            cards.foldLeft(0) { (acc, c) =>
                if c == CardLabel.Jack then acc + 1 // strength of J is 1
                else acc + c.labelStrength(true)
            }

    def handType: HandType = if withJoker then handTypeWithJoker else handTypeNoJoker

    private def handTypeNoJoker: HandType =
        val grouped = cards.groupBy(identity).view.mapValues(_.size).toMap
        val values  = grouped.values.toList
        if grouped.size == 1 then FiveOfAKind
        else if grouped.size == 2 && values.contains(4) then FourOfAKind
        else if grouped.size == 2 && values.contains(3) then FullHouse
        else if grouped.size == 3 && values.sorted == List(1, 1, 3) then ThreeOfAKind
        else if grouped.size == 3 && values.sorted == List(1, 2, 2) then TwoPair
        else if grouped.size == 4 && values.sorted == List(1, 1, 1, 2) then OnePair
        else HighCard

    private def handTypeWithJoker: HandType =
        if !cards.contains(CardLabel.Jack) then handTypeNoJoker
        else
            (List(Ace, Queen, King) ++ (2 to 10).map(Numbered(_)).toList)
                .map: c =>
                    val noJ    = cards.filterNot(_ == CardLabel.Jack)
                    val jcount = cards.size - noJ.size
                    Hand(List.fill(jcount)(c) ++ noJ, withJoker = true)
                .sorted
                .reverse
                .head.handType

object Hand:
    given Ordering[Hand] with
        def compare(x: Hand, y: Hand): Int =
            x.handType.strength.compare(y.handType.strength) match
            case 0 =>
                val xstrength = x.cards.map(_.labelStrength(x.withJoker))
                val ystrength = y.cards.map(_.labelStrength(y.withJoker))
                val result    = (xstrength lazyZip ystrength).map(_ - _).find(_ != 0).getOrElse(0)
                if result < 0 /* x < y */
                then -1
                else if result > 0 /* x > y */ then 1
                else 0
            case r => r

def toCardLabels(str: String) = str.map {
    case c if c.isDigit => CardLabel.Numbered(c.asDigit)
    case 'T'            => CardLabel.Numbered(10)
    case 'J'            => CardLabel.Jack
    case 'Q'            => CardLabel.Queen
    case 'K'            => CardLabel.King
    case 'A'            => CardLabel.Ace
}.toList

def buildInput = io.Source
    .fromResource("day7.txt")
    .getLines
    .map(_.split(" ").toList)
    .map(line => (Hand(toCardLabels(line.head)), line.last.toInt))
    .toList

def _part(input: List[(Hand, Int)], withJoker: Boolean) =
    input
        .map: (hand, i) =>
            (hand.copy(withJoker = withJoker), i)
        .sortBy(_._1)
        .zipWithIndex
        .map: (h, rank) =>
            h._2 * (rank + 1)
        .sum

def part1 = _part(buildInput, withJoker = false)
def part2 = _part(buildInput, withJoker = true)

@main def day7 =
    println("\nDay 7\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
