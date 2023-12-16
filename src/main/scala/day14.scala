package advent2023.day14

private val RoundedRock = 'O'
private val CubeRock    = '#'
private val EmptySpace  = '.'

val input: Array[Array[Char]] = scala.io.Source
    .fromResource("day14.txt")
    .getLines
    .toArray
    .map(_.toCharArray)

val rows = input.size
val cols = input.head.size

type Row = Int
type Col = Int

def moveNorth(pos: (Row, Col), grid: Array[Array[Char]]): Array[Array[Char]] =
    if pos._1 == 0 then grid
    else
        grid(pos._1)(pos._2) match

        // empty or cube => no move possible
        case EmptySpace | CubeRock => grid

        // rounded but the top is not empty to move => no move possible
        case RoundedRock if grid(pos._1 - 1)(pos._2) == RoundedRock || grid(pos._1 - 1)(pos._2) == CubeRock => grid

        // move and recurse
        case RoundedRock =>
            grid(pos._1)(pos._2) = EmptySpace
            grid(pos._1 - 1)(pos._2) = RoundedRock
            moveNorth((pos._1 - 1, pos._2), grid)

        // should not happen
        case c => scala.Function.const(grid)(println(s"got invalid $c"))

def printArray(a: Array[Array[Char]]) =
    a.foreach(row => println(row.mkString))

def part1 =
    val grid = input.zipWithIndex.foldLeft(input)((grid, row) =>
        (0 until cols).foldLeft(grid)((a, e) => moveNorth((row._2, e), grid))
    )
    (rows until 0 by -1).zipWithIndex.foldLeft(0) ((a, e) =>
        a + grid(e._2).filter(_ == RoundedRock).size * e._1
    )

def part2 = {}

@main def day14 =
    println("\nDay 14\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
