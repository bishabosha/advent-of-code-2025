package day08

import utils.*
import challenges.*
import scala.util.chaining.*
import stringmatching.regex.Interpolators.r
import scala.language.experimental.relaxedLambdaSyntax
import scala.util.boundary, boundary.break
import scala.reflect.ClassTag

@main def part1 = println(solve1(inputToday(), n = 1000))
@main def part2 = println(solve2(inputToday()))

type Point3D = (x: Int, y: Int, z: Int)

def dist(a: Point3D, b: Point3D): Double =
  val dx = Math.abs(b.x - a.x)
  val dy = Math.abs(b.y - a.y)
  val dz = Math.abs(b.z - a.z)
  Math.sqrt(math.pow(dx, 2) + math.pow(dy, 2) + math.pow(dz, 2))

def fastCombinations[A](as: IArray[A]): IArray[(A, A)] =
  // starting from outer index, pair with all inner indices starting from the next position,
  // then creep the outer index forward.
  // No need to go back to the start on each inner loop as those combinations are already covered.
  val buf = IArray.newBuilder[(A, A)]
  val n = as.length
  for i <- 0 until n do
    for j <- i + 1 until n do
      buf += (as(i) -> as(j))
  buf.result()

def calcDistances(input: String): (points: IArray[Point3D], conns: IArray[(Point3D, Point3D)]) =
  val r"${r"$xs%d,$ys%d,$zs%d"}...(\n)" = input.runtimeChecked
  val points = xs
    .lazyZip(ys)
    .lazyZip(zs)
    .map: (x, y, z) => (x = x, y = y, z = z)
    .toIArray
  (points, fastCombinations(points).sortBy(dist))

def include(a: Point3D, b: Point3D, acc: IArray[Set[Point3D]]): IArray[Set[Point3D]] =
  val merge = Set.newBuilder[Point3D]
  val rest = IArray.newBuilder[Set[Point3D]]
  for points <- acc do
    if points(a) || points(b) then merge ++= points
    else rest += points
  (rest += (merge += a += b).result()).result()

def solve1(input: String, n: Int): Long =
  val state0 = calcDistances(input)
  val (pairings, rest0) = state0.conns.splitAt(n)
  var acc = IArray.empty[Set[Point3D]]
  for (a, b) <- pairings do
    acc = include(a, b, acc)
  val full = acc.map(_.size).sortBy(-_) ++ IArray.fill(rest0.length * 2)(1)
  full.take(3).product

def solve2(input: String): BigInt =
  boundary:
    val state0 = calcDistances(input)
    val target = state0.points.length
    var acc = IArray.empty[Set[Point3D]]
    for (a, b) <- state0.conns do
      acc = include(a, b, acc)
      if acc.head.size == target then
        break(BigInt(a.x) * BigInt(b.x))
    sys.error("No solution found")
