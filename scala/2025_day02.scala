package day02

import challenges.*

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

def solve1(input: String): Long =
  solver(input, coveredByRange)

def solve2(input: String): Long =
  solver(input, coveredByRangeFull)

type Bucket = (start: Long, end: Long)
extension (b: Bucket)
  def rangeTest =
    val head = b.start
    val last = b.end
    (n: Long) => n >= head && n <= last

def solver(input: String, sieve: Bucket => collection.Set[Long]): Long =
  val ranges = input.linesIterator.next().split(",").map:
    case s"$a-$b" => a.toLong -> b.toLong
  ranges.map(sieve(_).sum).sum

def digitString(ds: Seq[Int]): Long =
  ds.foldLeft(0L)((acc, d) => acc * 10L + d)

def coveredByRange(r: Bucket): collection.Set[Long] =
  val sizeMin = digitCount(r.start)
  val sizeMax = digitCount(r.end)
  var seen = Set[Long]()
  for size <- sizeMin to sizeMax if size % 2 == 0 do
    seen ++= memoRepNums(size, size / 2).view.filter(r.rangeTest)
  seen

def coveredByRangeFull(r: Bucket): collection.Set[Long] =
  val sizeMin = digitCount(r.start)
  val sizeMax = digitCount(r.end)
  var seen = Set[Long]()
  for size <- sizeMin to sizeMax if size >= 2 do
    seen ++= repNumsFull(size).view.flatMap(_.filter(r.rangeTest))
  seen

def digitCount(n: Long): Int =
  if n == 0 then 1
  else Math.log10(n).toInt + 1

val _memoRepNums = collection.mutable.HashMap[(Int, Int), Array[Long]]()
def memoRepNums(size: Int, divisor: Int): Array[Long] =
  _memoRepNums.getOrElseUpdate((size, divisor), repNums(size, divisor))

def repNums(size: Int, divisor: Int): Array[Long] =
  val digits = Vector.fill(divisor)(0 to 9).flatten
  digits
    .combinations(divisor)
    .flatMap(_.permutations.filter(_.head != 0))
    .map(ds => digitString(Vector.fill(size / divisor)(ds).flatten))
    .toArray

def repNumsFull(size: Int): Seq[Array[Long]] =
  val divisors = (1 to size / 2).filter(size % _ == 0)
  divisors.map(memoRepNums(size, _))
