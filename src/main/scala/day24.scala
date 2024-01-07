package advent2023.day24

import z3.scala.*
import scala.util.Using
import scala.util.Using.Releasable

final case class Hailstone(id: Int, sx: Long, sy: Long, sz: Long, vx: Long, vy: Long, vz: Long)
case class TargetCoordinates(vars: Z3AST*)

given Releasable[Z3Context] with
    def release(resource: Z3Context): Unit = resource.delete()

val hailstones =
    scala.io.Source
        .fromResource("day24.txt")
        .getLines
        .zipWithIndex
        .map:
            case (s"$x, $y, $z @ $vx, $vy, $vz", idx) =>
                Hailstone(
                  idx,
                  x.trim.toLong,
                  y.trim.toLong,
                  z.trim.toLong,
                  vx.trim.toLong,
                  vy.trim.toLong,
                  vz.trim.toLong
                )
        .toVector

val LOW_RANGE = 200000000000000L
val HI_RANGE  = 400000000000000L

// https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
def intersects(sa: Hailstone, sb: Hailstone): Boolean =
    val (x1, y1, _) = (sa.sx, sa.sy, sa.sz)
    val (x2, y2, _) = (sa.sx + sa.vx, sa.sy + sa.vy, sa.sz + sa.vz)
    val (x3, y3, _) = (sb.sx, sb.sy, sb.sz)
    val (x4, y4, _) = (sb.sx + sb.vx, sb.sy + sb.vy, sb.sz + sb.vz)

    val den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val t0  = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    val u0  = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)

    if den == 0 then false // parallel
    else if
        (0 <= t0 && 0 <= den || 0 <= -t0 && 0 <= -den) && (0 <= u0 && 0 <= den || 0 <= -u0 && 0 <= -den)
    then
        val t = t0 / den.toDouble
        val x = x1 + t * (x2 - x1)
        val y = y1 + t * (y2 - y1)
        if x >= LOW_RANGE && x <= HI_RANGE && y >= LOW_RANGE && y <= HI_RANGE then true else false
    else false

def findTrajectory(stones: Vector[Hailstone]): Option[Vector[Long]] =

    val result = Using(new Z3Context()): ctx =>
        val solver = ctx.mkSolver()
        val outs =
            TargetCoordinates(Vector("ox", "oy", "oz", "vox", "voy", "voz").map(ctx.mkIntConst)*)
        stones.foreach(hs => buildConstraints(hs, outs, ctx, solver))

        if solver.check() == Some(true) then
            val model = solver.getModel()
            Some(
              outs
                  .vars
                  .toVector
                  .map(model.eval(_, true).get)
                  .map(ctx.getNumeralReal(_).numerator.longValue)
            )
        else None

    result.toOption.flatten

def buildConstraints(
    h1: Hailstone,
    targetCoordinates: TargetCoordinates,
    ctx: Z3Context,
    solver: Z3Solver
) =
    val i   = ctx.mkIntSort()
    val hid = ctx.mkIntConst(s"h${h1.id}")
    // t1 >= 0
    solver.assertCnstr(ctx.mkGE(hid, ctx.mkInt(0, ctx.mkIntSort())))

    val x1  = ctx.mkNumeral(h1.sx.toString, i)
    val vx1 = ctx.mkNumeral(h1.vx.toString, i)
    // x0 + t0 * vx0
    // x1 + t1 * vx1
    // x2 + t2 * vx2
    val fx1 = ctx.mkAdd(x1, ctx.mkMul(hid, vx1))

    // y0 + t0 * vy0
    // y1 + t1 * vy1
    // y2 + t2 * vy2
    val y1  = ctx.mkNumeral(h1.sy.toString, i)
    val vy1 = ctx.mkNumeral(h1.vy.toString, i)
    val fy1 = ctx.mkAdd(y1, ctx.mkMul(hid, vy1))

    // z0 + t0 * vz0
    // z1 + t1 * vz1
    // z2 + t2 * vz2
    val z1  = ctx.mkNumeral(h1.sz.toString, i)
    val vz1 = ctx.mkNumeral(h1.vz.toString, i)
    val fz1 = ctx.mkAdd(z1, ctx.mkMul(hid, vz1))

    val TargetCoordinates(ox, oy, oz, vox, voy, voz) = targetCoordinates

    // t0 * vox + ox = x0 + t0 * vx0
    // t1 * vox + ox = x1 + t1 * vx1
    // t2 * vox + ox = x2 + t2 * vx2

    // constraints
    val cs1 = ctx.mkAdd(ox, ctx.mkMul(hid, vox)) === fx1
    val cs2 = ctx.mkAdd(oy, ctx.mkMul(hid, voy)) === fy1
    val cs3 = ctx.mkAdd(oz, ctx.mkMul(hid, voz)) === fz1

    solver.assertCnstr(ctx.mkAnd(cs1, cs2, cs3))

def part1 =
    hailstones.combinations(2).count:
        case h1 +: h2 +: hs => intersects(h1, h2)
        case _              => ???

def part2 =
    findTrajectory(hailstones.take(3)) match
    case Some(Seq(ox, oy, oz, _, _, _)) => ox + oy + oz
    case _                              => throw new Exception("Cannot satisfy constraints")

@main def day24 =
    println("\nDay 24\n------------")
    println(s"Part 1: $part1") // 15318
    println(s"Part 2: $part2") // 870379016024859
