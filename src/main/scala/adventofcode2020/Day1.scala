package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.IntAccumulator

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1.txt").getLines.toList.map(_.toInt)

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[Int]): Int = {
    val result = lines.flatMap(x => lines.map(y => (x,y, x+y))).find(_._3 == 2020).get
    result._1 * result._2
  }

  def part2(lines: List[Int]): Int = {
    val result = lines.flatMap(x => lines.flatMap(y => lines.map(z => (x,y,z,x + y + z)))).find(_._4 == 2020).get
    result._1 * result._2 * result._3
  }

}

