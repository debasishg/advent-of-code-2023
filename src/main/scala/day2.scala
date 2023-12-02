package advent2023.day2

case class ColorCount(count: Int, color: String)

private def getColorCountsPerLine(line: String): (Int, List[ColorCount]) =
    val arr    = line.split(": ")
    val gameId = arr(0).split(" ")(1).toInt
    val groups = arr(1).split("; ").toList
    val colorCounts = groups.flatMap(_.split(", ").toList).map: s =>
        val split = s.split(" ")
        ColorCount(split(0).toInt, split(1))
    (gameId, colorCounts)

private def part1 =
    def anyExceeds(l: List[ColorCount]) =
        l.exists: cc =>
            cc.color == "red" && cc.count > 12 ||
                cc.color == "green" && cc.count > 13 ||
                cc.color == "blue" && cc.count > 14

    io.Source
        .fromResource("day2.txt")
        .getLines
        .foldLeft(0): (count, line) =>
            val (gameId, colorCounts) = getColorCountsPerLine(line)
            if !anyExceeds(colorCounts) then count + gameId else count

private def part2 =
    io.Source
        .fromResource("day2.txt")
        .getLines
        .map: line =>
            val (gameId, colorCounts) = getColorCountsPerLine(line)
            colorCounts.groupBy(_.color).map { case (color, counts) =>
                counts.map(_.count).max
            }.foldLeft(1)(_ * _)
        .sum

@main def run: Unit =
    println("\nDay 2\n------------")
    println(part1)
    println(part2)
