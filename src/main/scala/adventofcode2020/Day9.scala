package main.scala.adventofcode2020

import scala.io.Source

object Day9 {
  val preambleSize = 25
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day9.txt").getLines.toList.map(_.toLong)

    val part1Result = part1(lines)
    println(part1Result)
    println(part2(lines, part1Result))
  }

  def part1(lines: List[Long]): Long = {
    val preamble = scala.collection.mutable.Queue[Long]()

    lines
      .find(current => {
        val hasSumOrIsPreamble = preamble.length < preambleSize || preamble
          .combinations(2)
          .map(_.sum)
          .exists(_ == current)

        if (preamble.length == preambleSize) {
          preamble.dequeue()
        }
        preamble.enqueue(current)

        !hasSumOrIsPreamble
      })
      .get
  }

  def part2(lines: List[Long], target: Long): Long = {
    val linesAr = lines.toArray
    var windowStart = 0;
    var windowEnd = 0; //noninclusive
    var total = 0L;

    while (total != target || windowEnd - windowStart < 2) {
      if (total < target) {
        total += linesAr(windowEnd)
        windowEnd += 1
      } else {
        // it's more than target, remove from the window
        total -= linesAr(windowStart)
        windowStart += 1
      }
    }
    val subArray = linesAr.slice(windowStart, windowEnd)
    subArray.max + subArray.min
  }

}
