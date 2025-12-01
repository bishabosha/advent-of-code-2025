package day01

import challenges.*

import stringmatching.regex.Interpolators.r

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

def parseTurn(pos: Int, line: String): (pos: Int, turn: Int) =
  val turn = line match
    case s"L$num" => -num.toInt
    case s"R$num" => num.toInt
  (
    pos = (((pos + turn) % 100) + 100) % 100,
    turn = turn
  )

def solve1(input: String): Int =
  val (_, res) = input.linesIterator.foldLeft((50, 0)): (acc, line) =>
    val (pos, times) = acc
    val (newPos, turn) = parseTurn(pos, line)
    (newPos, times + (if newPos == 0 then 1 else 0))
  res

def solve2(input: String): Int =
  val (_, res) = input.linesIterator.foldLeft((50, 0)): (acc, line) =>
    val (pos, times) = acc
    val (newPos, turn) = parseTurn(pos, line)

    val rounds = turn.abs / 100
    val remainingTurn = turn % 100

    val extraCross = pos + remainingTurn match
      case p if p < 0 => pos > 0 || p <= -100
      case 0 => true
      case p => p >= 100

    val crosses = rounds + (if extraCross then 1 else 0)

    (newPos, times + crosses)
  res
