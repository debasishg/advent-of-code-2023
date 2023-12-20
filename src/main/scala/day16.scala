package advent2023.day16

enum Direction:
    case North, South, East, West

import Direction.*

type Row = Int
type Col = Int

val input: Array[Array[Char]] = scala.io.Source
    .fromResource("day16.txt")
    .getLines
    .toArray
    .map(_.toCharArray)

val rows = input.size
val cols = input.head.size

case class Pos(row: Row, col: Col)
case class Tile(pos: Pos, content: Char, var energized: Boolean = false)

val grid: Array[Array[Tile]] = input.zipWithIndex.map { (row, i) =>
    row.zipWithIndex.map { (content, j) =>
        Tile(Pos(i, j), content)
    }
}
def resetGrid = grid.foreach(_.foreach(_.energized = false))

case class Beam(pos: Pos, direction: Direction):
    def next(newDirection: Direction): Beam = newDirection match
    case North => Beam(Pos(pos.row - 1, pos.col), newDirection)
    case South => Beam(Pos(pos.row + 1, pos.col), newDirection)
    case East  => Beam(Pos(pos.row, pos.col + 1), newDirection)
    case West  => Beam(Pos(pos.row, pos.col - 1), newDirection)

    def advance(tileContent: Char): List[Beam] =
        (direction, tileContent) match
        case (North, '/')  => List(next(East))
        case (North, '\\') => List(next(West))
        case (North, '-')  => List(next(East), next(West))
        case (South, '/')  => List(next(West))
        case (South, '\\') => List(next(East))
        case (South, '-')  => List(next(East), next(West))
        case (East, '/')   => List(next(North))
        case (East, '\\')  => List(next(South))
        case (East, '|')   => List(next(North), next(South))
        case (West, '/')   => List(next(South))
        case (West, '\\')  => List(next(North))
        case (West, '|')   => List(next(North), next(South))
        case _             => List(next(direction))

def withinWalls(pos: Pos): Boolean =
    0 <= pos.row && pos.row < rows &&
        0 <= pos.col && pos.col < cols

@annotation.tailrec
def startMovement(starts: List[Beam], visited: List[Beam]): List[Beam] =
    starts match
    case Nil => visited
    case head :: next =>
        val tile = grid(head.pos.row)(head.pos.col)
        tile.energized = true
        val newVisited = head :: visited
        val newStarts =
            head.advance(tile.content).filter(b =>
                !newVisited.contains(b) && withinWalls(b.pos)
            ) ::: next
        startMovement(newStarts, newVisited)

def countEnergized: Int = grid.map(row => row.count(_.energized)).sum

val topBeams    = (0 until cols).map(col => Beam(Pos(0, col), South)).toList
val bottomBeams = (0 until cols).map(col => Beam(Pos(rows - 1, col), North)).toList
val leftBeams   = (0 until rows).map(row => Beam(Pos(row, 0), East)).toList
val rightBeams  = (0 until rows).map(row => Beam(Pos(row, cols - 1), West)).toList

def part1 =
    startMovement(List(Beam(Pos(0, 0), East)), List.empty)
    countEnergized

def part2 =
    resetGrid
    val allBeams = topBeams ::: bottomBeams ::: leftBeams ::: rightBeams
    allBeams.map: beam =>
        resetGrid
        startMovement(List(beam), List.empty)
        countEnergized
    .max

@main def day16 =
    println("\nDay 16\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
