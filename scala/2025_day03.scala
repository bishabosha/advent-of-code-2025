package day03

import challenges.*

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

def solve1(input: String): Long =
  input.linesIterator.map: bank =>
    val all = bank.map(_.asDigit)
    val maxCandidate = all.init.max
    val firstPos = all.init.indexOf(maxCandidate)
    val remaining = all.drop(firstPos + 1)
    val secondMax = remaining.max
    maxCandidate.toLong * 10L + secondMax.toLong
  .sum

def solve2(input: String): Long =
  input.linesIterator.map: bank =>
    val state0 = (batteries = bank.map(_.asDigit), candidates = IArray.newBuilder[Long])
    val state1 = (11 to 0 by -1).foldLeft(state0): (state, remaining) =>
      val range = state.batteries.dropRight(remaining)
      val candidate = range.max
      val firstPos = range.indexOf(candidate)
      (
        batteries = state.batteries.drop(firstPos + 1),
        candidates = state.candidates += candidate.toLong
      )
    state1.candidates.result().foldLeft(0L)((acc, d) => acc * 10L + d)
  .sum
