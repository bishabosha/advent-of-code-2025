package day06

import challenges.*
import scala.language.experimental.relaxedLambdaSyntax

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

enum Ops(val op: (Long, Long) => Long):
  case `*` extends Ops(_ * _)
  case `+` extends Ops(_ + _)

def solve1(input: String): Long =
  val columns = input.linesIterator.toArray
    .map: row => row.split("\\s+").filter(_.nonEmpty)
    .transpose

  val problems = columns.map: prob =>
    val nums = prob.init.map(_.toLong)
    val op = Ops.valueOf(prob.last)
    nums -> op

  problems
    .map: (nums, op) => nums.reduceLeft(op.op)
    .sum
end solve1

def solve2(input: String): Long =
  val rows = input.linesIterator.toArray
  val rowSizes = rows.map: row => row.split("\\s+").filter(_.nonEmpty).map(_.size)
  val colSizes = rowSizes.transpose.map(_.max)
  val cols = rows
    .map: row =>
      val buf = Array.newBuilder[String]
      var i = 0
      for size <- colSizes do
        buf += row.slice(i, i + size)
        i += size + 1
      buf.result()
    .transpose
  val problems = cols.map: prob =>
    val numPart = prob.init.map(_.toCharArray()).transpose
    val nums = numPart.map: col =>
      col.foldLeft(0L): (acc, d) => if d.isWhitespace then acc else acc * 10 + d.asDigit
    val op = Ops.valueOf(prob.last.filterNot(_.isWhitespace))
    nums -> op
  problems
    .map: (nums, op) => nums.reduceLeft(op.op)
    .sum
