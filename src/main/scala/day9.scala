package advent2023.day9

import scala.annotation.tailrec

val buildInput = io.Source
    .fromResource("day9.txt")
    .getLines
    .map(_.split(" ").toList.map(_.toLong))
    .toList

enum Mode:
    case Forward, Backward

@tailrec def extrapolate(l: List[Long], lastNos: List[Long], mode: Mode): List[Long] =
    val r = l.sliding(2).map(l => l.last - l.head).toList
    if r.forall(_ == 0) then lastNos.prepended(0)
    else
        extrapolate(
          r,
          lastNos.prepended(if mode == Mode.Forward then r.last else r.head),
          mode
        )

def part1 =
    buildInput.map: l =>
        l.last + extrapolate(l, List.empty[Long], Mode.Forward).sum
    .sum

def part2 =
    buildInput.map: l =>
        val x = extrapolate(l, List.empty, Mode.Backward) :+ l.head
        x.foldLeft(0L)((a, e) => e - a)
    .sum

@main def day9 =
    println("\nDay 9\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
