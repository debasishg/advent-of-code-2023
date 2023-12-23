package advent2023.day17

import scala.collection.mutable

enum Direction:
    case North, South, East, West

import Direction.*

type Row = Int
type Col = Int

val input: Array[Array[Int]] = scala.io.Source
    .fromResource("day17.txt")
    .getLines
    .map(_.map(_.asDigit).toArray).toArray

val rows = input.size
val cols = input.head.size

case class Position(row: Row, col: Col, direction: Direction):
    def nextForwardPos: Position = direction match
    case North => Position(row - 1, col, direction)
    case South => Position(row + 1, col, direction)
    case East  => Position(row, col + 1, direction)
    case West  => Position(row, col - 1, direction)

    def afterTurningPositions: Array[Position] = (direction match
    case North => Array(Position(row, col, West), Position(row, col, East))
    case South => Array(Position(row, col, West), Position(row, col, East))
    case East  => Array(Position(row, col, North), Position(row, col, South))
    case West  => Array(Position(row, col, North), Position(row, col, South))
    ).map(_.nextForwardPos)

    def isTarget(map: Array[Array[Int]]): Boolean = 
        (row == map.length - 1) && (col == map(row).length - 1)

    def isInBounds(map: Array[Array[Int]]): Boolean = 
        row >= 0 && col >= 0 && row < map.length && col < map(row).length


case class Crucible(position: Position, streak: Int, heatLoss: Int)
object Crucible:
    val start = Crucible(Position(0, 0, East), 0, 0)
    given Ordering[Crucible] with
        def compare(x: Crucible, y: Crucible): Int =
            y.heatLoss.compareTo(x.heatLoss)

def neighbors(
    map: Array[Array[Int]],
    crucible: Crucible,
    minStreak: Int,
    maxStreak: Int
): Array[Crucible] =
    val neighbors = mutable.ArrayBuffer.empty[Crucible]

    if crucible.streak < maxStreak then
        val newPos = crucible.position.nextForwardPos
        if newPos.isInBounds(map) then
            neighbors.addOne(Crucible(
              newPos,
              crucible.streak + 1,
              crucible.heatLoss + map(newPos.row)(newPos.col)
            ))

    if crucible.streak >= minStreak then
        crucible.position.afterTurningPositions
            .filter(_.isInBounds(map))
            .foreach(newPos =>
                neighbors.addOne(Crucible(newPos, 1, crucible.heatLoss + map(newPos.row)(newPos.col)))
            )

    neighbors.toArray

def dijkstra(map: Array[Array[Int]], start: Crucible, minStreak: Int, maxStreak: Int): Int =
    val visited   = mutable.Set.empty[(Position, Int)]
    val toVisit   = mutable.PriorityQueue.empty[Crucible]

    toVisit.enqueue(start)
    var target: Option[Crucible] = None

    while
        target.isEmpty
    do
        val curr        = toVisit.dequeue()
        val visitedHash = (curr.position, curr.streak)

        if curr.streak >= minStreak && curr.position.isTarget(map) then
            target = Some(curr)

        if !visited.contains(visitedHash) then
            visited.addOne(visitedHash)
            toVisit.enqueue(neighbors(map, curr, minStreak, maxStreak): _*)

    target.head.heatLoss

def part1 = dijkstra(input, Crucible.start, 0, 3)
def part2 = dijkstra(input, Crucible.start, 4, 10)

@main def day17 =
    println("\nDay 17\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
