package advent2023.day4

import cats.implicits.*

type CardNo = Int
type Count  = Int
final case class Card(no: CardNo, winning: Set[Int], input: Set[Int])

def parseLine(line: String): (Set[Int], Set[Int]) =
    val pattern     = """((\d+\s*)+)\s*\|((\s*\d+\s*)+)""".r
    val matchOption = pattern.findAllMatchIn(line)

    matchOption.map { matched =>
        val winning = matched.group(1).trim.split("\\s+").map(_.toInt).toList
        val input   = matched.group(3).trim.split("\\s+").map(_.toInt).toList
        (winning.toSet, input.toSet)
    }.toList.head

def cards = scala.io.Source
    .fromResource("day4.txt")
    .getLines
    .zipWithIndex
    .map: (line, index) =>
        val (winning, input) = parseLine(line)
        Card(index + 1, winning, input)

// matched numbers for a card
def matchedNumbers(card: Card) =
    card.winning.intersect(card.input)

def part1 =
    def cardWorth(s: Set[Int]) = math.pow(2, s.size - 1).toInt

    def computeLineValue(card: Card) =
        cardWorth(matchedNumbers(card))

    cards.map(computeLineValue).sum

val part2 = cards
    .foldLeft(Map.empty[CardNo, Count]): (acc, card) =>
        val count   = matchedNumbers(card).size
        val updated = acc.combine(Map(card.no -> 1))
        cards
            .drop(card.no)
            .take(count)
            .foldLeft(updated): (map, c) =>
                map.combine(Map(c.no -> map.getOrElse(card.no, 0)))
    .values
    .sum

@main def run: Unit =
    println("\nDay 4\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
