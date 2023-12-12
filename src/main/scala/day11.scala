package advent2023.day11

case class Coord(row: Long, column: Long)

def buildInputLazy(factor: Long) =
    val in = io.Source
        .fromResource("day11.txt")
        .getLines
        .toList

    val blankRows = findAllBlankRows(in)
    val blankColumns = findAllBlankColumns(in)

    in.zipWithIndex
        .foldLeft((Map.empty[Long, Coord], 0L)) { (a, e) =>
            val s             = e._1
            val i             = e._2
            val blankRowsPreceding = findNumberOfBlanksPreceding(blankRows, i)
            val galaxiesInRow = findIndexes(s, '#')
            a._1 ++ galaxiesInRow.zipWithIndex.map { case(g, idx) =>
                val blankColsPreceding = findNumberOfBlanksPreceding(blankColumns, g)
                (a._2 + idx, 
                    Coord(offset(blankRowsPreceding, i, factor), offset(blankColsPreceding, g, factor)))
            } -> (a._2 + galaxiesInRow.length)
        }._1

def offset(blanksPreceding: Long, index: Long, factor: Long) =
    if (blanksPreceding == 0) index
    else (index - blanksPreceding) + factor * blanksPreceding

def findNumberOfBlanksPreceding(blankRows: List[Long], index: Long) =
    blankRows.takeWhile(_ < index).size

def transposeStrings(input: List[String]): List[String] = {
  val listOfLists = input.map(_.toList)
  val transposedList = listOfLists.transpose
  transposedList.map(_.mkString)
}

def findAllBlankRows(input: List[String]) = {
  val indexedStrings = input.zipWithIndex
  val blankRows = indexedStrings.filter { case (s, _) =>
    findIndexes(s, '#').isEmpty
  }
  blankRows.map { case (_, index) => index.toLong }
}

def findAllBlankColumns(input: List[String]) = 
    findAllBlankRows(transposeStrings(input))

def findIndexes(inputString: String, charToFind: Char): List[Long] =
    val indexedChars = inputString.zipWithIndex
    val matchingPairs = indexedChars.filter { case (char, index) =>
        char == charToFind
    }
    val indexes = matchingPairs.map { case (_, index) => index.toLong }
    indexes.toList

def distance(c1: Coord, c2: Coord) =
    val rowdiff  = c2.row - c1.row
    val coldiff  = c2.column - c1.column
    val distance = rowdiff.abs + coldiff.abs
    distance

def computeSum(in: Map[Long, Coord]) =
    in.keys.toList.combinations(2).collect { case List(k1, k2) =>
        distance(in(k1), in(k2))
    }.sum

def part1 = 
    computeSum(buildInputLazy(2))

def part2 = 
    computeSum(buildInputLazy(1000000))

@main def day11 =
    println("\nDay 11\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
