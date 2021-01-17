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

  case class State(previousNumbers: Map[Int, Int], previousNumber: Int)
  def increment(prevState: State,
                index: Int,
                starterNumbers: Array[Int]): State = {
    val nextNumber =
      if (index < starterNumbers.length) {
        starterNumbers(index)
      } else {
        prevState.previousNumbers
          .get(prevState.previousNumber)
          .map(index - _ - 1)
          .getOrElse(0)
      }
    val newMap = if (index == 0) {
      // prevState.previousNumber wasn't in the sequence so don't add it.
      prevState.previousNumbers
    } else {
      prevState.previousNumbers + (prevState.previousNumber -> (index - 1))
    }

    State(newMap, nextNumber)
  }

  def part1(starterNumbers: Array[Int]): Int = {

    (0 until 2020)
      .foldLeft(State(Map[Int, Int](), 0)) { (prevState, index) =>
        increment(prevState, index, starterNumbers)
      }
      .previousNumber
  }
  def part2(starterNumbers: Array[Int]): Int = {
    (0 until 30000000)
      .foldLeft(State(Map[Int, Int](), 0)) { (prevState, index) =>
        increment(prevState, index, starterNumbers)
      }
      .previousNumber
  }

}
