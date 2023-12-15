package advent2023.day11

case class Coord(row: Long, column: Long)

def buildInput(factor: Long) =
    val in = io.Source
        .fromResource("day11.txt")
        .getLines
        .toList

    // only store indexes of blank rows and columns
    // adjust when you need to find the index of a galaxy
    val blankRows    = findAllBlankRows(in)
    val blankColumns = findAllBlankColumns(in)

    in.zipWithIndex
        .foldLeft((Map.empty[Long, Coord], 0L)): (a, e) =>
            val s                  = e._1
            val i                  = e._2
            val blankRowsPreceding = findNumberOfBlanksPreceding(blankRows, i)
            val galaxiesInRow      = findIndexes(s, '#')
            a._1 ++ galaxiesInRow.zipWithIndex.map { case (g, idx) =>
                val blankColsPreceding = findNumberOfBlanksPreceding(blankColumns, g)
                (
                  a._2 + idx,
                  Coord(
                    adjusted(blankRowsPreceding, i, factor),
                    adjusted(blankColsPreceding, g, factor)
                  )
                )
            } -> (a._2 + galaxiesInRow.length)
        ._1

def adjusted(blanksPreceding: Long, index: Long, factor: Long) =
    (index - blanksPreceding) + factor * blanksPreceding

def findNumberOfBlanksPreceding(blankRows: List[Long], index: Long) =
    blankRows.takeWhile(_ < index).size

def transpose(input: List[String]): List[String] =
    input.map(_.toList).transpose.map(_.mkString)

def findAllBlankRows(input: List[String]) =
    val blankRows = input.zipWithIndex.filter: (s, _) =>
        findIndexes(s, '#').isEmpty
    blankRows.map { case (_, index) => index.toLong }

def findAllBlankColumns(input: List[String]) =
    findAllBlankRows(transpose(input))

def findIndexes(inputString: String, charToFind: Char): List[Long] =
    val matchingPairs = inputString.zipWithIndex.filter: (char, _) =>
        char == charToFind
    matchingPairs.map(_._2.toLong).toList

// min distance is computed from the offset of row and column
def distance(c1: Coord, c2: Coord) =
    (c2.row - c1.row).abs + (c2.column - c1.column).abs

def sum(in: Map[Long, Coord]) =
    in.keys.toList.combinations(2).collect { case k1 :: k2 :: Nil =>
        distance(in(k1), in(k2))
    }.sum

def part1 =
    sum(buildInput(2))

def part2 =
    sum(buildInput(1000000))

@main def day11 =
    println("\nDay 11\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
