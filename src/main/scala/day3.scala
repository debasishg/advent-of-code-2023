package advent2023.day3

def findNumbers(line: String, row: Int): List[NumberCoordinates] =
    val regex = """\d+""".r
    regex.findAllMatchIn(line).map(m => (m.matched, m.start))
        .toList.map: (number, start) =>
            NumberCoordinates(number.toInt, row, start, number.size)

def findSymbols(line: String, row: Int): List[SymbolCoordinates] =
    def isSymbol(ch: Char) =
        (ch < '0' || ch > '9') && ch != '.'

    line.zipWithIndex.foldLeft(List.empty[SymbolCoordinates])((a, e) =>
        val symbol = e._1
        val pos    = e._2
        if isSymbol(symbol) then
            a :+ SymbolCoordinates(symbol.toString, row, pos)
        else a
    )

final case class NumberCoordinates(number: Int, row: Int, start: Int, size: Int)
final case class SymbolCoordinates(symbol: String, row: Int, pos: Int)

// check if the number is adjacent to any of the symbols
def checkIfAdjacent(
    ncord: NumberCoordinates,
    scords: List[SymbolCoordinates]
): Option[SymbolCoordinates] =
    scords.find: scord =>
        val SymbolCoordinates(s, srow, spos)          = scord
        val NumberCoordinates(n, nrow, nstart, nsize) = ncord

        val symbolOnEitherSide = (spos == nstart - 1 || spos == nstart + nsize)
        val symbolInBetween    = (spos >= nstart - 1 && spos <= nstart + nsize)

        (srow == nrow && symbolOnEitherSide) ||
        (srow == nrow + 1 && symbolInBetween) ||
        (srow == nrow - 1 && symbolInBetween)

val (symbols, numbers) = io.Source
    .fromResource("day3.txt")
    .getLines
    .zipWithIndex
    .foldLeft((List.empty[SymbolCoordinates], List.empty[NumberCoordinates]))((a, e) =>
        val (symbols, numbers) = a
        val (line, row)        = e
        val s                  = findSymbols(line, row)
        val n                  = findNumbers(line, row)
        (symbols ++ s, numbers ++ n)
    )

def part1 =
    val allAdjacents = numbers.filter(number => checkIfAdjacent(number, symbols).isDefined)
    allAdjacents.map(_.number).sum

def part2 =
    val adjacentToStars =
        numbers.foldLeft(Map.empty[SymbolCoordinates, List[NumberCoordinates]])((a, number) =>
            checkIfAdjacent(number, symbols) match
            case Some(symbol) if symbol.symbol == "*" =>
                val numbers = a.getOrElse(symbol, List.empty[NumberCoordinates])
                a + (symbol -> (numbers :+ number))
            case _ => a
        )
    adjacentToStars.filter(_._2.size == 2).map(_._2.map(_.number).product).sum

@main def run: Unit =
    println("\nDay 3\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
