package advent2023.day5

type SeedNo = Long

case class LongRange(start: Long, end: Long):
    def contains(n: Long) = start <= n && n <= end

case class MapRecord(sourceRange: LongRange, destinationRange: LongRange):
    def mappingFor(source: Long): Option[Long] =
        if sourceRange.contains(source) then
            val offset = source - sourceRange.start
            Some(destinationRange.start + offset)
        else None

def findFirstMatch(source: Long, mappings: List[MapRecord]): Option[Long] =
    mappings.collectFirst {
        case mr if mr.mappingFor(source).isDefined => mr.mappingFor(source).get
    }

case class SeedInfo(seeds: List[SeedNo], map: Map[String, List[MapRecord]])
object SeedInfo:
    def empty: SeedInfo = SeedInfo(List.empty, Map.empty)

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
val mappings      = seedInfo.map

val seedToSoil            = mappings("seed-to-soil")
val soilToFertilizer      = mappings("soil-to-fertilizer")
val fertilizerToWater     = mappings("fertilizer-to-water")
val waterToLight          = mappings("water-to-light")
val lightToTemperature    = mappings("light-to-temperature")
val temperatureToHumidity = mappings("temperature-to-humidity")
val humidityToLocation    = mappings("humidity-to-location")

def soilToLocationForASeed(seed: Long) =
    val soil        = findFirstMatch(seed, seedToSoil).getOrElse(seed)
    val fertilizer  = findFirstMatch(soil, soilToFertilizer).getOrElse(soil)
    val water       = findFirstMatch(fertilizer, fertilizerToWater).getOrElse(fertilizer)
    val light       = findFirstMatch(water, waterToLight).getOrElse(water)
    val temperature = findFirstMatch(light, lightToTemperature).getOrElse(light)
    val humidity    = findFirstMatch(temperature, temperatureToHumidity).getOrElse(temperature)
    val location    = findFirstMatch(humidity, humidityToLocation).getOrElse(humidity)
    location

def findMinLocation(seeds: List[Long]) =
    seeds.map(soilToLocationForASeed).min

def findMinLocation(seed: Long, length: Long) =
    var minimum = Long.MaxValue
    var i       = 0L
    while i < length do
        val location = soilToLocationForASeed(seed + i)
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
