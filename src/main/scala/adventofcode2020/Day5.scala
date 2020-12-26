package main.scala.adventofcode2020

import java.util.NoSuchElementException

import scala.io.Source

object Day5 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day5.txt").getLines().toList
    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[String]): Int =
    lines.map(convertToSeatId(_)).max


  def convertToSeatId(seatInstructions: String): Int = {
    val rowNumBinary = seatInstructions.substring(0, 7).toCharArray.map(ch => if (ch == 'F') '0' else '1').mkString
    val rowNumber = Integer.parseInt(rowNumBinary, 2)
    val seatNumBinary = seatInstructions.substring(7).toCharArray.map(ch => if (ch == 'L') '0' else '1').mkString
    val seatNum = Integer.parseInt(seatNumBinary, 2)
    rowNumber * 8 + seatNum
  }

  def part2(lines: List[String]): Int = {
    val filledSeats = lines.map(convertToSeatId(_)).toSet
    (0 to 127 * 8 + 7).find(i => filledSeats.contains(i - 1) && !filledSeats.contains(i) && filledSeats.contains(i+1))
      .get
  }
}

