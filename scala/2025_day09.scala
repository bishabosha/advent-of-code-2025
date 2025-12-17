package day09

import utils.*
import challenges.*
import scala.util.chaining.*
import scala.language.experimental.relaxedLambdaSyntax

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

type Point = (y: Long, x: Long)
type Line = (a: Point, b: Point)

def solve1(input: String): Long =
  val reds = input.linesIterator.toIArray.map: case s"$a,$b" => (y = a.toLong, x = b.toLong)
  reds.combinations(2)
    .map:
      case IArray(a, b) => ((a.y - b.y).abs + 1) * ((a.x - b.x).abs + 1)
    .max

// def split(a: (x: Long, y: Long), b: (x: Long, y: Long)): IArray[(x: Long, y: Long)] =

def solve2(input: String): Long =
  val reds = input.linesIterator.toIArray.map: case s"$a,$b" => (y = a.toLong, x = b.toLong)
  val points = reds.toSet
  val lines = reds.zip(reds.tail) :+ (reds.last, reds.head)
  0

def inferredPoints(points: Set[Point])(l: Line): IArray[Point] =
  val `tl->br` = vec(l.a, l.b)
  val `tl->tr`: Point = 0L -> `tl->br`.x
  val `tl->bl`: Point = `tl->br`.y -> 0L

  val tr: Point = (l.a.y + `tl->tr`.y) -> (l.a.x + `tl->tr`.x)
  val bl: Point = (l.a.y + `tl->bl`.y) -> (l.a.x + `tl->bl`.x)
  val (existTr, existBl) = (points(tr), points(bl))
  if existTr && existBl then IArray.empty // perfect square
  else
    if !existTr && !existBl then IArray(tr, bl)
    else if !existTr then IArray(bl)
    else IArray(tr)

def square(points: Set[Point])(l: Line): Boolean =
  val candidates = inferredPoints(points)(l)
  ???

def vec(a: Point, b: Point): Point =
  (y = b.y - a.y, x = b.x - a.x)

def dir(a: Point, b: Point): Point =
  val vec0 = vec(a, b)
  (y = vec0.y.sign, x = vec0.x.sign)

def reverse(v: Point): Point =
  (y = -v.y, x = -v.x)