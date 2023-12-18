package advent2023.day14

private val RoundedRock = 'O'
private val CubeRock    = '#'
private val EmptySpace  = '.'

enum Direction:
    case North, South, East, West

val input: Array[Array[Char]] = scala.io.Source
    .fromResource("day14.txt")
    .getLines
    .toArray
    .map(_.toCharArray)

val rows = input.size
val cols = input.head.size

type Row = Int
type Col = Int

def hash(in: Array[Array[Char]]) =
    in.map(_.mkString).mkString

// next position for direction
def getNextPos(dir: Direction, pos: (Row, Col)) =
    dir match
    case Direction.North => (pos._1 - 1, pos._2)
    case Direction.South => (pos._1 + 1, pos._2)
    case Direction.East  => (pos._1, pos._2 + 1)
    case Direction.West  => (pos._1, pos._2 - 1)

def move(
    direction: Direction,
    pos: (Row, Col)
): Unit =
    val moveEnd =
        direction match
        case Direction.North => pos._1 == 0
        case Direction.South => pos._1 == rows - 1
        case Direction.East  => pos._2 == cols - 1
        case Direction.West  => pos._2 == 0

    val (nextRow, nextCol) = getNextPos(direction, pos)
    if moveEnd then ()
    else
        input(pos._1)(pos._2) match

        // empty or cube => no move possible
        case EmptySpace | CubeRock => ()

        // rounded but the next position is not empty to move => no move possible
        case RoundedRock
            if input(nextRow)(nextCol) == RoundedRock || input(nextRow)(
              nextCol
            ) == CubeRock => ()

        // move and recurse
        case RoundedRock =>
            input(pos._1)(pos._2) = EmptySpace
            input(nextRow)(nextCol) = RoundedRock
            move(direction, (nextRow, nextCol))

        // should not happen
        case c => ???

// One complete move for the direction across the grid
def moveDirection(dir: Direction) =
    // for a west or north move start from the first row and first column of the grid
    if dir == Direction.North || dir == Direction.West then
        (0 until rows).foreach(row =>
            (0 until cols).foreach(e => move(dir, (row, e)))
        )
    // for a south move start from the last row and first column of the grid
    else if dir == Direction.South then
        (rows - 1 to 0 by -1).foreach(row =>
            (0 until cols).foreach(e => move(dir, (row, e)))
        )
    // for a east move start from the first row but end column
    else
        (0 until rows).foreach(row =>
            (cols - 1 to 0 by -1).foreach(e => move(dir, (row, e)))
        )

def cycle =
    moveDirection(Direction.North)
    moveDirection(Direction.West)
    moveDirection(Direction.South)
    moveDirection(Direction.East)

val memo = scala.collection.mutable.Map.empty[String, Int]

// the trick here is to find that the cycle repeats after a certain number of iterations
// need to find this repeating cycle and then just compute the remaining iterations
def repeatCycle(n: Int, cycleFound: Boolean): Unit =
    if n == 0 then ()
    // once the cycle has been found just recurse
    else if cycleFound then
        cycle
        repeatCycle(n - 1, true)
    else
        cycle
        // memoize till the cycle is found
        val h = hash(input)
        memo.get(h) match
        case Some(v) =>
            val cycleSize = v - n
            repeatCycle((n - 1) % cycleSize, true)
        case None =>
            memo += (h -> n)
            repeatCycle(n - 1, false)

def computeNorthLoad =
    (rows until 0 by -1).zipWithIndex.foldLeft(0)((a, e) =>
        a + input(e._2).filter(_ == RoundedRock).size * e._1
    )

def part1 =
    moveDirection(Direction.North)
    computeNorthLoad

def part2 =
    val times = 1000000000
    repeatCycle(times, false)
    computeNorthLoad

@main def day14 =
    println("\nDay 14\n------------")
    println(s"Part 1: $part1") // 109661
    println(s"Part 2: $part2") // 90176
