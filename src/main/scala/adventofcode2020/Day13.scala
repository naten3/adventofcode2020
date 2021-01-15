package main.scala.adventofcode2020

import main.scala.adventofcode2020.Day12.Direction.Direction

import scala.io.Source

object Day13 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13.txt").getLines.toArray
    val earliestTimestamp = lines(0).toInt
    val busIds = lines(1).split(",").filter(!_.equals("x")).map(_.toInt).toArray

    println(part1(earliestTimestamp, busIds))
    println(part2(earliestTimestamp, busIds))
  }

  def part1(earliestTimestamp: Int, busIds: Array[Int]): Int = {
    val (busId: Int, departureTime: Int) = busIds
      .map(bid => {
        val busDepartureTime =
          Iterator
            .from(1)
            .map(_ * bid)
            .find(_ > earliestTimestamp)
            .get
        (bid, busDepartureTime)
      })
      .minBy(_._2)

    busId * (departureTime - earliestTimestamp)
  }

  def part2(earliestTimestamp: Int, busIds: Array[Int]): Int = 0
}
