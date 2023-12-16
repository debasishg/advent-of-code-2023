package advent2023.day12

private val Operational = '.'
private val Damaged     = '#'
private val Unknown     = '?'

object Memo:
    def memoize[A1, A2, A3, B](fn: (A1, A2, A3) => B): (A1, A2, A3) => B =
        val cache = collection.mutable.Map.empty[(A1, A2, A3), B]
        (a1: A1, a2: A2, a3: A3) =>
            cache.getOrElseUpdate((a1, a2, a3), fn(a1, a2, a3))

// constrained search with memoization
val solver: (states: List[Char], counts: List[Int], damagesSeen: Int) => Long =
    Memo.memoize: (states, counts, damagesSeen) =>
        states match
        case Nil if (damagesSeen == 0 && counts.isEmpty) || counts == List(damagesSeen) => 1L
        case Nil                                                                        => 0L

        case Operational :: rest if counts.headOption.contains(damagesSeen) =>
            // got operational but we have already seen the required number of damaged ones
            // recurse on both states and counts
            solver(rest, counts.tail, 0)
        case Operational :: rest if damagesSeen == 0 =>
            // got operational but we have not seen any damaged ones
            // recurse on states
            solver(rest, counts, 0)
        case Operational :: rest => 0L

        case Damaged :: rest if counts.headOption.contains(damagesSeen) =>
            // got damaged but we have already seen all the counts
            0L
        case Damaged :: rest =>
            // got another damaged and we haven't yet seen all of them - add to the count
            solver(rest, counts, damagesSeen + 1)

        case Unknown :: rest =>
            // count as if it's damaged
            solver(Damaged +: rest, counts, damagesSeen) +
                // count as if it's operational
                solver(Operational +: rest, counts, damagesSeen)

        case _ => 0L

def buildInput(name: String): List[(List[Char], List[Int])] =
    scala.io.Source
        .fromResource(name)
        .getLines
        .map {
            case s"$record $damagedGroups" =>
                val damaged = damagedGroups.split(",").toList.map(_.toInt)
                record.toList -> damaged
        }
        .toList

def part1 =
    buildInput("day12.txt")
        .map: (states, counts) =>
            solver(states, counts, 0)
        .sum

def part2 =
    buildInput("day12.txt")
        .map: (states, counts) =>
            solver(
              List.fill(5)('?' +: states).flatten.tail,
              List.fill(5)(counts).flatten,
              0
            )
        .sum

@main def day12 =
    println("\nDay 12\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
