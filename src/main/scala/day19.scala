package advent2023.day19

import scala.annotation.tailrec

enum Category:
    case X, M, A, S

import Category.*

extension (s: String)
    def toCategory: Category = s match
    case "x" => X
    case "m" => M
    case "a" => A
    case "s" => S

enum FinalState:
    case Accept, Reject

import FinalState.*

extension (s: String)
    def toFinalState: Option[FinalState] = s match
    case "A" => Some(Accept)
    case "R" => Some(Reject)
    case _   => None

enum ComparisonOp:
    case GT, LT

import ComparisonOp.*

extension (s: String)
    def toComparisonOp: ComparisonOp = s match
    case ">" => GT
    case "<" => LT

type Rating       = Long
type WorkflowName = String

final case class Part(x: Rating, m: Rating, a: Rating, s: Rating):
    def sum = x + m + a + s
    def cvalue(c: Category): Rating =
        c match
        case Category.X => x
        case Category.M => m
        case Category.A => a
        case Category.S => s

final case class Condition(c: Category, op: ComparisonOp, r: Rating):
    def eval(value: Rating): Boolean = op match
    case ComparisonOp.GT => value > r
    case ComparisonOp.LT => value < r

final case class Conditional(
    condition: Condition,
    trueBranch: WorkflowName | FinalState,
    falseBranch: WorkflowName | FinalState | Conditional
):
    @tailrec
    def eval(part: Part): WorkflowName | FinalState =
        // optimization for workflows that don't need evaluation of the condition
        // e.g. lnx{m>1548:A,A}
        if trueBranch.isInstanceOf[FinalState] && falseBranch.isInstanceOf[
              FinalState
            ] && trueBranch == falseBranch
        then trueBranch.asInstanceOf[FinalState]
        else
            val cv = part.cvalue(condition.c)
            condition.eval(cv) match
            case true => trueBranch
            case false => falseBranch match
                case w: WorkflowName => w
                case f: FinalState   => f
                case c: Conditional  => c.eval(part)

object Conditional:
    def toTrueBranch(s: String): FinalState | WorkflowName = s.toFinalState match
    case Some(state) => state
    case None        => s

    def toFalseBranch(s: String): FinalState | WorkflowName | Conditional = s.toFinalState match
    case Some(state) => state
    case None =>
        if s.contains(":") then Conditional.fromString(s)
        else s

    def fromString(s: String) = s match
    case s"$c<$r:$t,$f" if c.size == 1 =>
        Conditional(
          Condition(c.toCategory, LT, r.toInt),
          toTrueBranch(t),
          toFalseBranch(f)
        )
    case s"$c>$r:$t,$f" if c.size == 1 =>
        Conditional(
          Condition(c.toCategory, GT, r.toInt),
          toTrueBranch(t),
          toFalseBranch(f)
        )

final case class Workflow(name: WorkflowName, conditional: Conditional):
    // process the part through the workflow and get the final state
    def eval(part: Part): FinalState | WorkflowName =
        conditional.eval(part)

object Workflow:
    def countInterpreter(
        parts: Parts,
        start: WorkflowName | FinalState | Conditional
    ): Long =
        start match
        case Accept => parts.count()
        case Reject => 0L

        case Conditional(Condition(cat, op, rating), trueBranch, falseBranch) =>
            val (trueParts, falseParts) = op match
            case LT => parts.split(cat, rating)
            case GT => parts.split(cat, rating + 1).swap
            trueParts.map(countInterpreter(_, trueBranch)).getOrElse(0L) + falseParts.map(
              countInterpreter(
                _,
                falseBranch
              )
            ).getOrElse(0L)

        case name: WorkflowName =>
            workflows(name) match
            case Workflow(
                  name,
                  cond @ Conditional(Condition(cat, op, rating), trueBranch, falseBranch)
                ) =>
                countInterpreter(parts, cond)

lazy val parts: List[Part] = scala.io.Source
    .fromResource("day19-parts.txt")
    .getLines
    .toList
    .map:
        case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

lazy val workflows: Map[WorkflowName, Workflow] = scala.io.Source
    .fromResource("day19-wfs.txt")
    .getLines
    .toList
    .map:
        case s"$name{$conditional}" => (name, Workflow(name, Conditional.fromString(conditional)))
    .toMap

def toTerminalState(part: Part, start: Workflow): FinalState =
    start.eval(part) match
    case f: FinalState        => f
    case wfName: WorkflowName => toTerminalState(part, workflows(wfName))

case class Range(from: Long, until: Long):
    assert(from < until)
    def count() = until - from

object Range:
    def safe(from: Long, until: Long): Option[Range] =
        if from < until then Some(Range(from, until)) else None

case class Parts(x: Range, m: Range, a: Range, s: Range):
    def count() = x.count() * m.count() * a.count() * s.count()

    private def newRangeFor(category: Category, r: Range) =
        category match
        case X => copy(x = r)
        case M => copy(m = r)
        case A => copy(a = r)
        case S => copy(s = r)

    private def cvalue(category: Category) =
        category match
        case X => x
        case M => m
        case A => a
        case S => s

    def split(category: Category, value: Rating): (Option[Parts], Option[Parts]) =
        val currentRange = cvalue(category)
        (
          Range.safe(currentRange.from, value).map(newRangeFor(category, _)),
          Range.safe(value, currentRange.until).map(newRangeFor(category, _))
        )

def part1 =
    val start = workflows("in")
    parts
        .map(p => (p, toTerminalState(p, start)))
        .filter(_._2 == Accept)
        .map(_._1.sum)
        .sum

extension [T](part: (T, T)) private inline def swap: (T, T) = (part._2, part._1)

def part2 =
    val ratingsRange = Range(1, 4001)
    Workflow.countInterpreter(
      parts = Parts(
        x = ratingsRange,
        m = ratingsRange,
        a = ratingsRange,
        s = ratingsRange
      ),
      start = "in"
    )

@main def day19 =
    println("\nDay 19\n------------")
    println(s"Part 1: $part1") // 287054
    println(s"Part 2: $part2") // 131619440296497
