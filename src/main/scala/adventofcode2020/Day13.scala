package main.scala.adventofcode2020

import main.scala.adventofcode2020.Day12.Direction.Direction

import scala.io.Source

object Day13 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13.txt").getLines.toArray
    val earliestTimestamp = lines(0).toInt
    val busIds = lines(1).split(",")

    println(part1(earliestTimestamp, busIds))
    println(part2(earliestTimestamp, busIds))
  }

  def part1(earliestTimestamp: Int, busIds: Array[String]): Int = {
    val busIdInts = busIds.filter(!_.equals("x")).map(_.toInt)
    val (busId: Int, departureTime: Int) = busIdInts
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

  // extended euclidean
  def computeInverse(c: Long, d: Long): Long = {
    var a = c
    var b = d
    var m = b
    var t: Long = 0
    var q: Long = 0
    var x = 0L
    var y = 1L

    while (a > 1) {
      q = a / b
      t = b
      b = a % b
      a = t
      t = x
      x = y - q * x
      y = t
    }
    if (y < 0) {
      y + m
    } else {
      y
    }
  }

  def part2(earliestTimestamp: Int, busIds: Array[String]): Long = {
    val busIdsWithRemainders = busIds.zipWithIndex
      .filter(!_._1.equals("x"))
      .map(busWithIndex => {
        val busId = busWithIndex._1.toInt
        val index = busWithIndex._2

        (busId, (busId - index) % busId)
      })

    // chinese remainder theorem
    val product =
      busIdsWithRemainders.foldLeft(1L)((acc, next) => acc * next._1)
    val partialProducts = busIdsWithRemainders.map(product / _._1)
    val inverses = partialProducts.indices.map(
      i => computeInverse(partialProducts(i), busIdsWithRemainders(i)._1)
    )
    val sum = partialProducts.indices.foldLeft(0L)(
      (acc, i) =>
        acc + partialProducts(i) * inverses(i) * busIdsWithRemainders(i)._2
    )
    sum % product
  }
}
