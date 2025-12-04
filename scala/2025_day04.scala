package day04

import challenges.*

@main def part1 = println(solve1(inputToday()))
@main def part2 = println(solve2(inputToday()))

object data:
  opaque type Grid = IArray[IArray[Char]]
  object Grid:
    def apply(rows: IArray[IArray[Char]]): Grid = rows
    extension (g: Grid)
      def height: Int = g.length
      def width: Int = g(0).length
      def isFree(y: Int, x: Int): Boolean = test('.')(y, x)
      def isRoll(y: Int, x: Int): Boolean = test('@')(y, x)
      def test(char: Char)(y: Int, x: Int): Boolean =
        if y < 0 || y >= height || x < 0 || x >= width then false
        else g(y)(x) == char
      def flip(candidates: Iterable[(y: Int, x: Int)]): Grid =
        val newRows = g.map(row =>
          val newRow = new Array[Char](row.length)
          row.copyToArray(newRow)
          newRow
        )
        for (y, x) <- candidates do
          newRows(y)(x) = '.'
        Grid(newRows.map(IArray.unsafeFromArray(_)))

import data.Grid

val directions = IArray(
  (y = 0, x = 1),
  (y = 1, x = 0),
  (y = 0, x = -1),
  (y = -1,x =  0),
  (y = 1, x = 1),
  (y = 1, x = -1),
  (y = -1,x =  1),
  (y = -1,x =  -1)
)

def solve1(input: String): Int =
  given Grid = Grid(IArray.from(input.linesIterator.map(IArray.from(_))))
  val candidates = scala.collection.mutable.HashSet.empty[(y: Int, x: Int)]
  for
    y <- 0 until summon[Grid].height
    x <- 0 until summon[Grid].width
    if summon[Grid].isFree(y, x)
  do
    candidates ++= adjacentRolls(y, x)
  candidates.count(isValidRoll(_, _))

def adjacentPositions(y: Int, x: Int): IArray[(y: Int, x: Int)] =
  directions.map(d => (y + d.y, x + d.x))

def isValidRoll(y: Int, x: Int)(using g: Grid): Boolean =
  adjacentPositions(y, x).count(g.isRoll(_, _)) < 4

def adjacentRolls(y: Int, x: Int)(using g: Grid): IArray[(y: Int, x: Int)] =
  adjacentPositions(y, x).filter(g.isRoll(_, _))

def solve2(input: String): Int =
  var root: Grid = Grid(IArray.from(input.linesIterator.map(IArray.from(_))))
  val candidates = scala.collection.mutable.HashSet.empty[(y: Int, x: Int)]
  var removedNow = 0
  var total = 0
  while
    given Grid = root
    for
      y <- 0 until summon[Grid].height
      x <- 0 until summon[Grid].width
      if summon[Grid].isFree(y, x)
    do
      candidates ++= adjacentRolls(y, x)
    removedNow = candidates.filterInPlace(isValidRoll(_, _)).size
    total += removedNow
    root = root.flip(candidates)
    removedNow > 0
  do candidates.clear()
  total
