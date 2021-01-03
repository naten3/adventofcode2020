package main.scala.adventofcode2020

import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10.txt").getLines.toList.map(_.toInt)

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[Int]): Int = {
    val sortedLines = lines.sorted
    val linesWithAdapters = 0 +: sortedLines :+ (sortedLines.last + 3)
    val oneAndThreeJumps = linesWithAdapters
      .sliding(2)
      .foldLeft((0, 0))((totals, nextPair) => {
        if (nextPair.last - nextPair.head == 1) {
          (totals._1 + 1, totals._2)
        } else {
          (totals._1, totals._2 + 1)
        }
      })
    oneAndThreeJumps._1 * oneAndThreeJumps._2
  }

  def part2(lines: List[Int]): Long = {
    val sortedLines = lines.sorted
    val linesWithAdapters = 0 +: sortedLines :+ (sortedLines.last + 3)
    val jumps = linesWithAdapters
      .sliding(2)
      .map(pair => pair.last - pair.head)

    val oneJumpGroups = jumps
      .foldLeft((List[Int](), 0))((acc, next) => {
        val (groups, runningCount) = acc
        if (next == 1) {
          (groups, runningCount + 1)
        } else {
          val newGroups =
            if (runningCount > 0) {
              runningCount +: groups
            } else {
              groups
            }
          (newGroups, 0)
        }
      })
      ._1

    val jumpMultiplierMap = Map((1 -> 1), (2 -> 2), (3 -> 4), (4 -> 7))

    oneJumpGroups.foldLeft(1L)(
      (product, next) => product * jumpMultiplierMap(next)
    )
  }

}
