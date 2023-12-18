package advent2023.day15

val input = scala.io.Source
    .fromResource("day15.txt")
    .getLines
    .toList
    .head
    .split(",")
    .toList

def hash(s: String) =
    s.foldLeft(0)((a, e) => ((a + e.toInt) * 17) % 256)

case class Lens(label: String, focalLength: Int)

def part1 = input.map(hash).sum

def part2 =
    val boxes = Array.fill[List[Lens]](256)(Nil)

    def addLens(label: String, focalLength: Int) =
        val h = hash(label)

        boxes(h).indexWhere(_.label == label) match
        case -1 => boxes(h) = boxes(h) :+ Lens(label, focalLength)
        case i  => boxes(h) = boxes(h).updated(i, Lens(label, focalLength))

    def removeLens(label: String) =
        val h = hash(label)
        boxes(h) = boxes(h).filter(_.label != label)

    def focusingPower(boxIndex: Int, lensIndex: Int, lens: Lens): Int =
        (boxIndex + 1) * (lensIndex + 1) * lens.focalLength

    for in <- input do
        in match
        case s"$label-"             => removeLens(label)
        case s"$label=$focalLength" => addLens(label, focalLength.toInt)

    val focusingPowers =
        for
            (box, boxIndex)   <- boxes.zipWithIndex
            (lens, lensIndex) <- box.zipWithIndex
        yield focusingPower(boxIndex, lensIndex, lens)

    focusingPowers.sum

@main def day15 =
    println("\nDay 15\n------------")
    println(s"Part 1: $part1") // 515974
    println(s"Part 2: $part2")
