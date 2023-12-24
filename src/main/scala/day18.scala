package advent2023.day18

enum Direction:
    case U, D, R, L

import Direction.*

extension (s: String)
    def toDirection: Direction = s match
    case "U" | "3" => U
    case "D" | "1" => D
    case "L" | "2" => L
    case "R" | "0" => R

type Row = Long
type Col = Long

final case class Coordinate(row: Row, col: Col)
final case class DigLine(direction: Direction, size: Int)

def getNextCoordinate(coordinate: Coordinate, digLine: DigLine): Coordinate =
    import digLine.*
    direction match
    case U => Coordinate(coordinate.row - size, coordinate.col)
    case D => Coordinate(coordinate.row + size, coordinate.col)
    case L => Coordinate(coordinate.row, coordinate.col - size)
    case R => Coordinate(coordinate.row, coordinate.col + size)

def coordinates(lines: List[DigLine]) = lines.foldLeft(List(Coordinate(0, 0))):
    case (coordinates, digLine) =>
        val nextCoordinate = getNextCoordinate(coordinates.head, digLine)
        nextCoordinate :: coordinates

val input = scala.io.Source
    .fromResource("day18.txt")
    .getLines
    .toList

lazy val parsePart1: List[DigLine] = input
    .map:
        case s"$dir $size (#$color)" => DigLine(dir.toDirection, size.toInt)
    .toList

lazy val parsePart2: List[DigLine] = input
    .map:
        case s"$dir $size (#$color)" => DigLine(color.drop(5).toDirection, Integer.parseInt(color.take(5), 16))
    .toList

def shoelace(coordinates: List[Coordinate]): Long = 
    coordinates.sliding(2).map {
        case List(Coordinate(x1, y1), Coordinate(x2, y2)) => (y1 + y2) * (x1 - x2)
        case x => 0
    }.sum / 2

def perimeter(lines: List[DigLine]): Long = lines.map(_.size).sum + 1

def part1 =
    val lines = parsePart1
    shoelace(coordinates(lines)) + perimeter(lines) / 2 + 1

def part2 = 
    val lines = parsePart2
    shoelace(coordinates(lines)) + perimeter(lines) / 2 + 1

@main def day18 =
    println("\nDay 18\n------------")
    println(s"Part 1: $part1") // 70026
    println(s"Part 2: $part2") // 68548301037382
