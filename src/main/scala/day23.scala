package advent2023.day23

import scala.collection.mutable

enum Direction:
    case R, L, U, D

import Direction.*

val input =
    scala.io.Source
        .fromResource("day23.txt")
        .getLines
        .map(line => line.toArray)
        .toArray

val height  = input.size
val rowSize = input(0).size

case class Position(row: Int, col: Int)

def neighbors(row: Int, col: Int, grid: Array[Array[Char]]): Set[Position] =
    val n = List(
      (row - 1, col),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col)
    )
    n.filter((row, col) =>
        row >= 0 && row < height && col >= 0 && col < rowSize && grid(row)(col) != '#'
    ).map((row, col) => Position(row, col)).toSet

val start = Position(0, 1)
val end   = Position(height - 1, rowSize - 2)

// we can apply edge contraction at the points of interest to reduce the graph
// at every point of interest, we keep the weight, which is the path length from the previous point of interest
// so instead of traversing each node we tarverse each point of interest
// at the start we add the `start` and `end` nodes with weight 0. Besides them every node with >= 3 neighbors
// is a point of interest
def pointsOfInterest(grid: Array[Array[Char]]) =
    val points = mutable.ListBuffer(start, end)
    (0 until height).foreach { r =>
        (0 until rowSize).foreach { c =>
            if grid(r)(c) != '#' && neighbors(r, c, grid).size >= 3 then
                points += Position(r, c)
        }
    }
    points.toList

val allowedDirs = Map(
  "^" -> List((-1, 0)),
  "v" -> List((1, 0)),
  "<" -> List((0, -1)),
  ">" -> List((0, 1)),
  "." -> List((-1, 0), (1, 0), (0, -1), (0, 1))
)

val dirsForPart2 = allowedDirs(".")

// we construct the graph of points of interest
// it's a representation as adjacency list - one node mapping to it's weighted neighbors
def makeGraph(
    points: List[Position],
    grid: Array[Array[Char]]
): Map[Position, Map[Position, Int]] =

    def isValid(x: Int, y: Int) =
        def boundsCheck =
            x >= 0 && x < height && y >= 0 && y < rowSize
        def forestCheck = grid(x)(y) != '#'
        boundsCheck && forestCheck

    val graph = Map[Position, mutable.Map[Position, Int]](points.map(p =>
        (p, mutable.Map.empty[Position, Int])
    ).toSeq*)

    points.foreach: sp =>
        val stack = mutable.Stack((0, sp))
        val seen  = mutable.Set(sp)

        while !stack.isEmpty do
            val (dist, p) = stack.pop()
            if dist != 0 && points.contains(p) then
                graph(sp) += (p -> dist)
            else
                // List((-1, 0), (1, 0), (0, -1), (0, 1)).foreach:
                dirsForPart2.foreach:
                    case (dr, dc) =>
                        val nr = p.row + dr
                        val nc = p.col + dc
                        if isValid(nr, nc) && !seen.contains(Position(nr, nc))
                        then
                            stack.push((dist + 1, Position(nr, nc)))
                            seen += Position(nr, nc)
    graph.map((k, v) => (k, v.toMap))

val seen = mutable.Set.empty[Position]
def dfs(pt: Position, graph: Map[Position, Map[Position, Int]]): Float =
    if pt == end then 0
    else
        var m = Float.NegativeInfinity
        seen += pt
        graph(pt).foreach { nx =>
            if !seen.contains(nx._1) then
                m = math.max(m, dfs(nx._1, graph) + graph(pt)(nx._1))
        }
        seen -= pt
        m

object PathFinder:

    def findAllPaths(
        matrix: Array[Array[Char]],
        start: (Int, Int),
        end: (Int, Int)
    ): List[List[(Int, Int)]] =

        def isValidMove(x: Int, y: Int, dir: Direction): Boolean =
            def boundsCheck =
                x >= 0 && x < matrix.length && y >= 0 && y < matrix(0).length
            def dirCheck =
                (dir == R && matrix(x)(y) != '<') ||
                    (dir == L && matrix(x)(y) != '>') ||
                    (dir == U && matrix(x)(y) != 'v') ||
                    (dir == D && matrix(x)(y) != '^')

            def forestCheck = matrix(x)(y) != '#'

            boundsCheck && forestCheck && dirCheck

        def dfs(
            current: (Int, Int),
            path: List[(Int, Int)],
            visited: Set[(Int, Int)]
        ): List[List[(Int, Int)]] =
            if current == end then
                // Reached the destination, add the current path
                List(path.reverse)
            else
                val neighbors =
                    List((0, 1, R), (1, 0, D), (0, -1, L), (-1, 0, U)) // Right, Down, Left, Up
                val paths =
                    for
                        (dx, dy, dir) <- neighbors
                        next = (current._1 + dx, current._2 + dy)
                        if isValidMove(next._1, next._2, dir) && !visited(next)
                        newPath = dfs(next, next :: path, visited + next)
                    yield newPath

                paths.flatten

        // Start the DFS from the given starting point
        dfs(start, List(start), Set(start))

def part1 =
    PathFinder.findAllPaths(input, (0, 1), (height - 1, rowSize - 2))
        .map(_.size)
        .max - 1

def part2 =
    val graph = makeGraph(pointsOfInterest(input), input)
    dfs(start, graph)

@main def day23 =
    println("\nDay 23\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
