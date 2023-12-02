package advent2023.day1

import scala.annotation.tailrec

private def part1 =
    def boundingInt(s: String): Option[Int] =
        for
            first  <- s.find(_.isDigit).map(_.toString.toInt)
            second <- s.findLast(_.isDigit).map(_.toString.toInt)
        yield 10 * first + second

    io.Source
        .fromResource("day1.txt")
        .getLines
        .flatMap(boundingInt)
        .sum

private def part2 =

    @tailrec def allIndexes(
        main: String,
        sub: String,
        from: Int = 0,
        result: List[Int] = Nil
    ): List[Int] =
        val index = main.indexOf(sub, from)
        if index == -1 then result
        else allIndexes(main, sub, index + 1, index :: result)

    val digitsInWords =
        List(
          "zero",
          "one",
          "two",
          "three",
          "four",
          "five",
          "six",
          "seven",
          "eight",
          "nine"
        )

    val digits = (0 to 9).toList.map(_.toString)

    val digitsIndex =
        digitsInWords.zipWithIndex.toMap ++ digits.zipWithIndex.toMap

    val substrings = digitsInWords ++ digits

    def calibrationFor(
        mainString: String
    ): Option[Int] =
        val indices = substrings.flatMap: substring =>
            allIndexes(mainString, substring).map(startIndex =>
                (startIndex, startIndex + substring.length - 1, substring)
            )

        if indices.nonEmpty then
            for
                first  <- digitsIndex.get(indices.minBy(_._1)._3)
                second <- digitsIndex.get(indices.maxBy(_._2)._3)
            yield first * 10 + second
        else None

    io.Source
        .fromResource("day1.txt")
        .getLines
        .flatMap: line =>
            calibrationFor(line)
        .sum

@main def run: Unit =
    println("\nDay 1\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
