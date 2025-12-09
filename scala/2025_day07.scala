package day07

import challenges.*
import scala.util.chaining.*
import scala.language.experimental.relaxedLambdaSyntax

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

def solve1(input: String): Int =
  val grid = input.linesIterator.map(_.toCharArray.pipe(IArray.from(_))).pipe(IArray.from(_))
  val valid = 0 to grid.size
  val state = (pos = Set.empty[Int], splits = 0)
  val state0 = grid.foldLeft(state): (state, row) =>
    if state.pos.isEmpty then
      // first row
      (pos = Set(row.indexOf('S')), splits = 0)
    else
      val candidates = state.pos.filter(row(_) == '^')
      if candidates.isEmpty then
        state
      else
        val next = candidates.foldLeft(Set.empty[Int]): (acc, pos) =>
          acc ++ IArray(pos - 1, pos + 1).filter(valid.contains(_))
        (pos = state.pos -- candidates ++ next, splits = state.splits + candidates.size)
  state0.splits

def solve2(input: String): Long =
  val grid = input.linesIterator.map(_.toCharArray.pipe(IArray.from(_))).pipe(IArray.from(_))
  val valid = 0 to grid.size
  val paths0 = grid.foldLeft(Map.empty[Int, Long]): (paths, row) =>
    if paths.isEmpty then
      // first row
      Map(row.indexOf('S') -> 1L)
    else
      val candidates = IArray.from(paths.keySet.filter(row(_) == '^'))
      if candidates.isEmpty then
        paths
      else
        val projection = candidates
          .map: pos => pos -> IArray(pos - 1, pos + 1).filter(valid.contains(_))
          .toMap

        val destinations = IArray.from(projection.values.flatten).distinct
        val olds = destinations.flatMap: col => paths.get(col).map(count => col -> count)
        val nexts = candidates.flatMap: col =>
          val count = paths(col)
          projection(col).map(_ -> count)
        val combined = (olds ++ nexts)
          .groupMapReduce((col, _) => col)((_, count) => count)(_ + _)

        paths -- candidates ++ combined
  paths0.values.sum
