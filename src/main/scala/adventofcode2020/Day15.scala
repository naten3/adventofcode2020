package main.scala.adventofcode2020

import scala.io.Source

object Day15 {
  def main(args: Array[String]): Unit = {
    val starterNumbers =
      Array
        .from(
          Source
            .fromResource("day15.txt")
            .getLines
            .next
            .split(",")
        )
        .map(_.toInt)

    println(part1(starterNumbers))
    println(part2(starterNumbers))
  }

  def increment(previousNumbers: scala.collection.mutable.Map[Int, Int],
                previousNumber: Int,
                index: Int,
                starterNumbers: Array[Int]): Int = {
    val nextNumber =
      if (index < starterNumbers.length) {
        starterNumbers(index)
      } else {
        previousNumbers
          .get(previousNumber)
          .map(index - _ - 1)
          .getOrElse(0)
      }
    if (index != 0) {
      previousNumbers.addOne((previousNumber, index - 1))
    }
    nextNumber
  }

  def part1(starterNumbers: Array[Int]): Int = {
    val previousNumbers = scala.collection.mutable.Map[Int, Int]()
    (0 until 2020)
      .foldLeft(0) { (previousNumber, index) =>
        increment(previousNumbers, previousNumber, index, starterNumbers)
      }

  }
  def part2(starterNumbers: Array[Int]): Int = {
    val previousNumbers = scala.collection.mutable.Map[Int, Int]()
    (0 until 30000000)
      .foldLeft(0) { (previousNumber, index) =>
        increment(previousNumbers, previousNumber, index, starterNumbers)
      }
  }

}
