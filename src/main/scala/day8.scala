package advent2023.day8

import scala.util.control.Breaks.*

case class Input(lrs: List[LR], lines: List[(String, (String, String))])

val buildInput = io.Source
    .fromResource("day8.txt")
    .getLines
    .filterNot(_.isEmpty)
    .foldLeft(Input(Nil, Nil)) {
        case (acc, s"$key = ($left, $right)") =>
            acc.copy(lines = acc.lines :+ (key, (left, right)))
        case (acc, line) =>
            acc.copy(lrs = line.map {
                case 'L' => L
                case 'R' => R
            }.toList)
    }

enum LR:
    case L, R

import LR.*

def foldLeftWithEarlyExit[A, B](
    list: LazyList[A],
    initialValue: B
)(f: (B, A) => B)(exitCondition: B => Boolean): B =
    var result = initialValue

    breakable {
        for element <- list do
            result = f(result, element)
            if exitCondition(result) then break
    }
    result

val rl       = buildInput.lrs
val repeated = LazyList.continually(rl).flatten

def findPathToFinal(start: String, finalStateFn: String => Boolean) =
    foldLeftWithEarlyExit(repeated, (0L, start)) { (acc, direction) =>
        val (count, current) = acc
        if finalStateFn(current) then acc
        else
            buildInput.lines.collectFirst {
                case (from, (toL, toR)) if from == current =>
                    direction match
                    case L => (count + 1L, toL)
                    case R => (count + 1L, toR)
            }.getOrElse((count, current))
    }((_, exit) => finalStateFn(exit))

val allStarts = buildInput.lines.filter(_._1.endsWith("A")).map(_._1).toList

def lcm(a: BigInt, b: BigInt): BigInt =
    (a * b).abs / a.gcd(b)

def part1 = findPathToFinal("AAA", _ == "ZZZ")._1

// lcm trick from Alex
def part2 =
    allStarts
        .map(findPathToFinal(_, _.endsWith("Z")))
        .map: (l, _) =>
            BigInt(l)
        .reduce(lcm(_, _))

@main def day8 =
    println("\nDay 8\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
