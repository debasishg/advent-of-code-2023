package advent2023.day5

import scala.collection.immutable.ListMap

type SeedNo = Long

case class LongRange(start: Long, end: Long):
    def contains(n: Long) = start <= n && n <= end

    // Interval strictly before another? True if the end of this is less than the start of other
    def before(other: LongRange) = end < other.start

    // Interval strictly after another? True if the start of this is greater than the end of other
    def after(other: LongRange) = start > other.end

    def overlapping(other: LongRange) = ???

    def intersection(other: LongRange) =
        val start = if this.start > other.start then this.start else other.start
        val end   = if this.end < other.end then this.end else other.end
        LongRange(start, end)

    def intersects(other: LongRange) =
        contains(other.start) || contains(other.end) || other.contains(start) || other.contains(end)

case class MapRecord(sourceRange: LongRange, destinationRange: LongRange):
    def mappingFor(source: Long): Option[Long] =
        if sourceRange.contains(source) then
            val offset = source - sourceRange.start
            Some(destinationRange.start + offset)
        else None

/** find matching ranges
  *
  * if sourceRange = (40, 75) and mappings = List(MapRecord(LongRange(40, 55), LongRange(12, 27)),
  * MapRecord(LongRange(56, 75), LongRange(28, 47))) then output LongRange(12, 47)
  *
  * if sourceRange = (40, 75) and mappings = List(MapRecord(LongRange(40, 55), LongRange(12, 27)),
  * MapRecord(LongRange(60, 75), LongRange(28, 43))) then output LongRange(12, 47)
  *
  * @param sourceRange
  * @param mappings
  * @return
  */
def findMatchingRanges(sourceRange: LongRange, mappings: List[MapRecord]) = ???

def findFirstMatch(source: Long, mappings: List[MapRecord]): Option[Long] =
    mappings.collectFirst {
        case mr if mr.mappingFor(source).isDefined => mr.mappingFor(source).get
    }

case class SeedInfo(seeds: List[SeedNo], map: ListMap[String, List[MapRecord]])
object SeedInfo:
    def empty: SeedInfo = SeedInfo(List.empty, ListMap.empty)

def getMapName(line: String): Option[String] =
    if line.contains(" map:") then line.split(" ").headOption
    else None

def buildSeedInfo = io.Source
    .fromResource("day5.txt")
    .getLines
    .foldLeft((SeedInfo.empty, "")): (acc, e) =>
        val (a, mapName) = acc
        if e.trim.isEmpty() then acc
        else if e.startsWith("seeds: ") then
            (a.copy(seeds = e.split(" ").toList.tail.map(_.toLong)), mapName)
        else
            getMapName(e) match
            case Some(mapName) =>
                (a.copy(map = a.map + (mapName -> List.empty)), mapName)
            case None =>
                val SeedInfo(seeds, map) = a
                val (destinationStartRange, sourceStartRange, rangeLength) =
                    e.split(" ").toList match
                    case List(destinationStartRange, sourceStartRange, rangeLength) =>
                        (destinationStartRange.toLong, sourceStartRange.toLong, rangeLength.toLong)
                    case _ => (0L, 0L, 0L)

                val mapRecord = MapRecord(
                  LongRange(sourceStartRange, sourceStartRange + rangeLength - 1),
                  LongRange(destinationStartRange, destinationStartRange + rangeLength - 1)
                )

                val updatedMap = map.updatedWith(mapName) {
                    case Some(list) => Some(list :+ mapRecord)
                    case None       => Some(List(mapRecord))
                }
                (a.copy(map = updatedMap), mapName)

val (seedInfo, _) = buildSeedInfo
val seeds         = seedInfo.seeds
val mappings      = seedInfo.map.values

def soilToLocation(seed: Long) = mappings.foldLeft(seed): (acc, list) =>
    findFirstMatch(acc, list).getOrElse(acc)

def findMinLocation(seeds: List[Long]) =
    seeds.map(soilToLocation).min

def findMinLocation(seed: Long, length: Long) =
    var minimum = Long.MaxValue
    var i       = 0L
    while i < length do
        val location = soilToLocation(seed + i)
        i += 1
        if location < minimum then
            minimum = location
    minimum

def part1 = findMinLocation(seeds)

def part2 =
    seeds.sliding(2, 2).map: group =>
        val seed  = group.head
        val range = group.last
        findMinLocation(seed, range)
    .min

@main def run: Unit =
    println("\nDay 5\n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
