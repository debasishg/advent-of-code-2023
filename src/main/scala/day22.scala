package advent2023.day22

import scala.collection.mutable
import scala.math.*

val input =
    scala.io.Source
        .fromResource("day22.txt")
        .getLines
        .zipWithIndex
        .map:
            case (s"$fx,$fy,$fz~$tx,$ty,$tz", _) =>
                Array(
                  fx.toInt,
                  fy.toInt,
                  fz.toInt,
                  tx.toInt,
                  ty.toInt,
                  tz.toInt
                )
        .toArray

def overlaps(a: Array[Int], b: Array[Int]) =
    max(a(0), b(0)) <= min(a(3), b(3)) && max(a(1), b(1)) <= min(a(4), b(4))

def afterFall(bricks: Array[Array[Int]]) =
    val sorted = bricks.sortBy(_(2))
    for (brick, idex) <- sorted.zipWithIndex do
        var mz = 1
        for check <- sorted.take(idex) do
            if overlaps(brick, check) then
                mz = max(mz, check(5) + 1)
        brick(5) -= brick(2) - mz
        brick(2) = mz
    sorted.sortBy(_(2))

val settled = afterFall(input)
// key supports values
val supports =
    mutable.Map.empty[Int, Set[Int]] ++ (0 until settled.size).map(i => i -> Set.empty[Int])
// key is supported by values
val supportedBy =
    mutable.Map.empty[Int, Set[Int]] ++ (0 until settled.size).map(i => i -> Set.empty[Int])

def part1 =
    for (upper, j) <- settled.zipWithIndex do
        for (lower, i) <- settled.zipWithIndex.take(j) do
            if overlaps(lower, upper) && upper(2) == lower(5) + 1 then
                supports(i) = supports.getOrElse(i, Set.empty) + j
                supportedBy(j) = supportedBy.getOrElse(j, Set.empty) + i

    var total = 0

    (0 until settled.size).foreach(i =>
        if supports(i).map(j => supportedBy(j).size >= 2).forall(_ == true) then
            total += 1
    )
    total

def part2 =
    var total = 0
    (0 until settled.size).foreach(i =>
        val q = mutable.ArrayDeque.empty[Int]
        supports(i).foreach(j => if supportedBy(j).size == 1 then q.append(j))
        val falling = mutable.Set((q.toSet + i).toSeq*)

        while !q.isEmpty do
            val j = q.removeHead()
            for k <- supports(j) -- falling do
                if supportedBy(k).subsetOf(falling) then
                    q.append(k)
                    falling += k
        total += falling.size - 1
    )
    total

@main def day22 =
    println("\nDay 22\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
