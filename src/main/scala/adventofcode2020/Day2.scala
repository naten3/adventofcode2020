package main.scala.adventofcode2020

import scala.io.Source

object Day2 {
  case class PasswordLine(min: Int, max: Int, letter: Char, password: String)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2.txt").getLines.toList
    val passwordLines = processInput(lines)

    println(part1(passwordLines))
    println(part2(passwordLines))
  }

  def part1(lines: List[PasswordLine]): Int = {
    lines.count(l => {
      val matchCount = l.password.toCharArray.foldLeft(0) { (acc, letter) => if (letter == l.letter) 1 + acc else acc }
      matchCount <= l.max && matchCount >= l.min
    })
  }

  def part2(lines: List[PasswordLine]): Int = {
    lines.count(l =>
      l.password.charAt(l.min - 1) == l.letter ^ l.password.charAt(l.max - 1) == l.letter
    )
  }

  def processInput(lines: List[String]):List[PasswordLine] = {
      val inputLine = raw"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r

      lines.map {
        case inputLine(min, max, letter, password) => PasswordLine(min.toInt, max.toInt, letter.charAt(0), password)
      }
    }

}

