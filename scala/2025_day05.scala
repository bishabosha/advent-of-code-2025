package day05

import challenges.*
import stringmatching.regex.Interpolators.r
import scala.collection.immutable.Queue

import scala.language.experimental.relaxedLambdaSyntax

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

// unfortunately Scala NumericRange[Long] requires that the diff from start to end is
// representable as an Int, so doesnt support the kind of wide ranges we need here.
case class LongRange(start: Long, end: Long):
  def contains(value: Long): Boolean =
    value >= start && value <= end
  def merge(other: LongRange): Option[LongRange] =
    if other.start <= end && other.end >= start then
      Some(LongRange(Math.min(start, other.start), Math.max(end, other.end)))
    else
      None
  def size: Long = end - start + 1

extension (i: Long) infix def to(other: Long): LongRange =
  LongRange(i, other)

def solve1(input: String): Int =
  val r"${r"${r"$ins%L"}-${r"$outs%L"}"}...(\n)\n\n${r"$ids%L"}...(\n)" = input.runtimeChecked
  val ranges = ins.lazyZip(outs).map: (in, out) => in to out
  ids.count: id => ranges.exists(_.contains(id))

def solve2(input: String): Long =
  val r"${r"${r"$ins%L"}-${r"$outs%L"}"}...(\n)\n\n$_" = input.runtimeChecked
  val ranges = ins.lazyZip(outs).map: (in, out) => in to out
  def loop(
      current: LongRange,
      stable: Boolean,
      explore: Queue[LongRange],
      nexts: Queue[LongRange],
      finals: Seq[LongRange]
  ): Seq[LongRange] =
    if explore.isEmpty then
      if nexts.isEmpty then finals :+ current
      else
        val (current0, explore1) = nexts.dequeue
        if stable then
          loop(current0, stable = true, explore1, Queue.empty, finals :+ current)
        else
          loop(current0, stable = true, explore1.enqueue(current), Queue.empty, finals)
    else
      val (e, explore1) = explore.dequeue
      current.merge(e) match
        case Some(current0) =>
          loop(current0, stable = false, explore1, nexts, finals)
        case None =>
          loop(current, stable, explore1, nexts.enqueue(e), finals)
  val (current, explore) = ranges.to(Queue).dequeue
  val finals = loop(current, stable = true, explore, nexts = Queue.empty, finals = Vector.empty)
  finals.map(_.size).sum
