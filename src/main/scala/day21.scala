package advent2023.day21

import scala.collection.mutable

enum Step:
    case Rock, GardenPlot

import Step.*

extension (s: String)
    def toStep: Step = s match
    case "#"       => Rock
    case "." | "S" => GardenPlot

val input: Array[Array[Step]] =
    scala.io.Source
        .fromResource("day21.txt")
        .getLines
        .toArray
        .map(_.split("").map(_.toStep))

type Row = Int
type Col = Int
final val startPosition = Coordinate(65, 65)

final case class Coordinate(row: Row, col: Col):
    // offset for infinite grid
    private def transform(x: Int, y: Int) =
        val m = x % y
        if m >= 0 then m else m + y

    def neighbors(grid: Array[Array[Step]]): Set[Coordinate] =
        val rows = grid.size
        val cols = grid.head.size

        def isPlot(coord: Coordinate) =
            val trow = transform(coord.row, rows)
            val tcol = transform(coord.col, cols)
            grid(trow)(tcol) match
            case GardenPlot => true
            case _          => false

        Set(
          Coordinate(row - 1, col),
          Coordinate(row + 1, col),
          Coordinate(row, col + 1),
          Coordinate(row, col - 1)
        ).filter(isPlot)

enum EvenOdd:
    case Even, Odd

import EvenOdd.*

/** Breadth first search on an infinite grid to find the shortest path to all reachable locations
  * within the number of steps. Returns a map of coordinates to the number of steps required to
  * reach that coordinate along with whether we needed an even or odd number of steps.
  */
def bfs(
    start: Coordinate,
    grid: Array[Array[Step]],
    steps: Int
): Map[Coordinate, Int] =
    val distances = mutable.Map.empty[Coordinate, (Int, EvenOdd)]
    distances += (start -> (0, Even))

    val _ = (0 until steps).foldLeft(Vector(start)):
        case (acc, step) =>
            val newDistance = distances(acc.head)._1 + 1
            acc.flatMap(_.neighbors(grid))
                .filter(n =>
                    if !distances.contains(n) || newDistance < distances(n)._1 then
                        distances += ((n, (newDistance, if (step + 1) % 2 == 0 then Even else Odd)))
                        true
                    else false
                )

    distances.toMap.filter:
        case (k, v) =>
            if steps % 2 == 0 then v._2 == Even else v._2 == Odd
    .map((k, v) => (k, v._1))

extension (seq: Vector[Int])
    def nOrderDifference(n: Int): Vector[Int] =
        if n == 0 then seq
        else
            val diff = seq.nOrderDifference(n - 1)
            diff.tail.zip(diff).map(_ - _)

    def differencesUpTo(order: Int): Vector[Vector[Int]] =
        (0 to order).map(seq.nOrderDifference).toVector

def part1 =
    bfs(
      startPosition,
      input,
      64
    ).values.size

/** Important to note that the relation between the number of plots visited and the number of steps
  * is quadratic. This can be visualized from the data. Hence we need to solve a quadratic equation
  * to find the number of locations visited within the number of steps specified, i.e. 26501365 we
  * wil solve the quadratic equation using the finite difference method.
  *
  * The equation can be written as: f(n) = an^2 + bn + c, where n is the number of steps and f(n) is
  * the number of locations visited. Using our bfs method we can find the number of locations
  * visited for a given number of steps. The fact that the first finite difference is not 0
  * indicates that the relation is non-linear dn the fact that the second finite difference is
  * constant implies it's quadratic. Below we assert that the third finite difference is 0. The rest
  * is solving the equation using the finite difference method.
  *
  * Now a bit of data analysis :
  *
  *   - It's a square grid of width 131, the center being (66, 66)
  *   - So it takes 65 steps to reach the edge from the center
  *   - We need to determine all positions reachable with 26501365 steps. Note 26501365 = 65 + 131 *
  *     n, where n is the number of tile lengths that we can move away from the centre in a straight
  *     line, given 26501365 steps.
  *   - so we need to find out how many locations can be covered if we start from the center, which
  *     is 65 away from the edge, and cover (26501365 - 65) / 131 tile lengths. This we input to the
  *     quadratic equation below and solve for the number of locations.
  */
def part2 =
    val steps = (0 to 3).map(65 + _ * 131)
    val sizes = steps.map(
      bfs(startPosition, input, _).size
    ).toVector

    val Vector(_, oneDiff, twoDiff, threeDiff) = sizes.differencesUpTo(3)

    // second difference constant => quadratic
    assert(twoDiff.tail.forall(_ == twoDiff.head))

    // calculate a, b, c for an^2 + bn + c
    val a    = twoDiff.head / 2
    val b    = oneDiff.head - a
    val c    = sizes.head
    val poly = (n: Long) => a * n * n + b * n + c

    poly((26501365 - 65) / 131)

@main def day21 =
    println("\nDay 21\n------------")
    println(s"Part 1: $part1") // 3764
    println(s"Part 2: $part2") // 622926941971282
