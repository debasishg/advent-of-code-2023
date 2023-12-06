package advent2023.day6

import cats.implicits.*

def buildInput = io.Source
    .fromResource("day6.txt")
    .getLines
    .map(_.split(":")(1).trim.replaceAll(" +", " ").split(" ").map(_.toLong).toList)
    .toList

def margin(input: Map[Long, Long]) = input.map { case (t, d) =>
    waysToWin(t, d)
}.product

def waysToWin(time: Long, distance: Long) =
    (1L to time - 1L).map { t =>
        val pushTime        = t
        val travelTime      = time - pushTime
        val speed           = pushTime
        val distanceCovered = speed * travelTime
        if distanceCovered > distance then 1 else 0
    }.sum

def part1 =
    val in  = buildInput
    val map = (in.head zip in.last).toMap
    margin(map)

def part2 =
    val in = buildInput.map(_.foldMap(_.toString)).map(_.toLong)
    margin(Map(in.head -> in.last))

@main def day6 =
    println("\nDay 6\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
