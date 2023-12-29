package advent2023.day20

import scala.collection.mutable
import scala.util.boundary
import boundary.break

object MathUtils:
    def gcd(n1: Long, n2: Long): Long =
        if n2 == 0 then n1
        else gcd(n2, n1 % n2)

    def lcm(n1: Long, n2: Long): Long = Math.abs(n1 * n2) / gcd(n1, n2)

    def lcm(numbers: Vector[Long]): Long =
        require(
          numbers.length >= 2,
          "At least 2 numbers needed"
        )
        numbers.reduce(lcm(_, _))

enum ModuleState:
    case On, Off

enum Pulse:
    case Low, High

sealed trait Module:
    def name: String
    def destinations: Vector[String]
    def handlePulse(source: String, input: Pulse): Option[Pulse]

case class Broadcaster(val name: String, val destinations: Vector[String]) extends Module:
    override def handlePulse(source: String, input: Pulse): Option[Pulse] = Some(input)

case class Button(val name: String, val destinations: Vector[String]) extends Module:
    override def handlePulse(source: String, input: Pulse): Option[Pulse] = Some(Pulse.Low)

case class FlipFlop(val name: String, val destinations: Vector[String]) extends Module:
    private var state: ModuleState = ModuleState.Off

    override def handlePulse(source: String, input: Pulse): Option[Pulse] = input match
    case Pulse.High => None
    case Pulse.Low =>
        state match
        case ModuleState.On =>
            state = ModuleState.Off
            Some(Pulse.Low)
        case ModuleState.Off =>
            state = ModuleState.On
            Some(Pulse.High)

case class Conjunction(val name: String, val destinations: Vector[String], inputs: Vector[String])
    extends Module:
    private val recentPulses: mutable.Map[String, Pulse] = inputs
        .map(_ -> Pulse.Low)
        .to(mutable.Map)

    override def handlePulse(source: String, input: Pulse): Option[Pulse] =
        recentPulses(source) = input
        recentPulses.values.forall(_ == Pulse.High) match
        case true => Some(Pulse.Low)
        case _    => Some(Pulse.High)

val part2TargetModuleName = "rx"

def lines =
    scala.io.Source
        .fromResource("day20.txt")
        .getLines
        .toList

def input(lines: List[String]) =
    val conjunctions = mutable.Set.empty[String]
    val modules = lines
        .map:
            case s"broadcaster -> $destinations" =>
                ("broadcaster" -> Broadcaster("broadcaster", destinations.split(", ").toVector))
            case s"%$name -> $destinations" =>
                (name -> FlipFlop(name, destinations.split(", ").toVector))
            case s"&$name -> $destinations" =>
                conjunctions += name
                (name -> Conjunction(
                  name,
                  destinations.split(", ").toVector,
                  Vector.empty[String]
                ))
        .to(mutable.Map)

    // todo : process inside out for efficiency
    conjunctions.foreach(c =>
        val conjunction = modules(c).asInstanceOf[Conjunction]
        modules.foreach { case (name, module) =>
            val destinations = module.destinations
            destinations.filter(_ == conjunction.name).foreach { n =>
                val conj = modules(n).asInstanceOf[Conjunction]
                modules(n) = Conjunction(
                  conjunction.name,
                  conjunction.destinations,
                  name +: conj.inputs
                )
            }
        }
    )
    modules.toMap

type Source     = String
type ModuleName = String

// start has received pulse from source
def once(start: String, source: String, pulse: Pulse, input: Map[String, Module]) =
    val inQueue              = mutable.Queue.empty[(ModuleName, Source, Pulse)]
    var highPulseCount: Long = 0
    var lowPulseCount: Long  = 1

    inQueue += ((start, source, pulse))
    while !inQueue.isEmpty do
        val (moduleName, source, pulse) = inQueue.dequeue()
        val module                      = input.get(moduleName)
        module match
        case Some(mod) =>
            mod.handlePulse(source, pulse).foreach(p =>
                inQueue ++= mod.destinations.map(d =>
                    if p == Pulse.Low then lowPulseCount += 1
                    else if p == Pulse.High then highPulseCount += 1
                    (d, moduleName, p)
                )
            )
        case None => ()

    (highPulseCount, lowPulseCount)

def go(start: String, source: String, pulse: Pulse, count: Int, input: Map[String, Module]) =
    val (hi, lo) = (0 until count)
        .map(_ => once(start, source, pulse, input))
        .foldLeft((0L, 0L)):
            case ((high, low), (h, l)) =>
                (high + h, low + l)
    hi * lo

def findSource(target: String, input: Map[String, Module]): Module =
    input
        .filter(_._2.destinations.contains(target))
        .head
        ._2

def onceRX(
    start: String,
    source: String,
    pulse: Pulse,
    targets: Vector[String],
    countMap: mutable.Map[String, Long],
    count: Long,
    input: Map[String, Module]
) =
    val inQueue = mutable.Queue.empty[(ModuleName, Source, Pulse)]

    inQueue += ((start, source, pulse))
    while !inQueue.isEmpty do
        val (moduleName, source, pulse) = inQueue.dequeue()
        if targets.contains(source) && pulse == Pulse.High then
            countMap += ((source, count))
        val module = input.get(moduleName)
        module match
        case Some(mod) =>
            mod.handlePulse(source, pulse).foreach(p =>
                inQueue ++= mod.destinations.map(d =>
                    (d, moduleName, p)
                )
            )
        case None => ()

    countMap

def goRX(start: String, source: String, pulse: Pulse, input: Map[String, Module]) =
    findSource(part2TargetModuleName, input) match
    case Conjunction(_, _, inputs) =>
        var map = mutable.Map.empty[String, Long]
        boundary:
            for buttonPress <- 1 to Int.MaxValue do
                map = onceRX(
                  start,
                  source,
                  pulse,
                  inputs,
                  map,
                  buttonPress,
                  input
                )
                if map.size == inputs.size then
                    break(map.toMap)

            Map.empty[String, Long]

    case _ => throw new Error("Nope")

def part1 =
    val parsedInput = input(lines)
    go("broadcaster", "Button", Pulse.Low, 1000, parsedInput)

// 1. find predecessor of "rx" => "rg" (conjunction)
// 2. "rg" is in the destination list of 4 conjunction modules
// 3. if we need to send Low to "rx", "rg" must receive High from all 4 conjunctions
// 4. find the button press count for all 4 conjunctions to receive High and compute the lcm
def part2 =
    val parsedInput = input(lines)
    val m           = goRX("broadcaster", "Button", Pulse.Low, parsedInput)
    m.values.toVector
        .foldLeft(1L)(MathUtils.lcm)

@main def day20 =
    println("\nDay 20\n------------")
    println(s"Part 1: $part1") // 808146535
    println(s"Part 2: $part2") // 224602953547789
