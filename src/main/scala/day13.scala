package advent2023.day13

type Tile    = '.' | '#'
type Line    = Vector[Tile]
type Pattern = Vector[Line]

def buildInput =
    scala.io.Source
        .fromResource("day13.txt")
        .getLines
        .toList
        .mkString("\n")

def parseInput(input: String): Vector[Pattern] =
    val patterns = input.split(raw"\R\R").toVector
    patterns.map: patternStr =>
        patternStr.split(raw"\R").toVector.map: lineStr =>
            lineStr.collect[Tile] { case tile: Tile => tile }.toVector

def reflectionFor(pattern: Pattern): Option[Int] =
    (1 until pattern.size).find: i =>
        val (leftPart, rightPart) = pattern.splitAt(i)
        leftPart.reverse.zip(rightPart).forall(_ == _)

def smudgedReflectionFor(pattern: Pattern): Option[Int] =
    (1 until pattern.size).find: i =>
        val (leftPart, rightPart) = pattern.splitAt(i)
        val smudges = leftPart.reverse
            .zip(rightPart)
            .map((l1, l2) => l1.zip(l2).count(_ != _))
            .sum
        smudges == 1

def part1 =
    parseInput(buildInput)
        .flatMap: pattern =>
            reflectionFor(pattern).map(100 * _).orElse(reflectionFor(pattern.transpose))
        .sum

def part2 =
    parseInput(buildInput)
        .flatMap: pattern =>
            smudgedReflectionFor(pattern).map(100 * _).orElse(
              smudgedReflectionFor(pattern.transpose)
            )
        .sum

@main def day13 =
    println("\nDay 13\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
