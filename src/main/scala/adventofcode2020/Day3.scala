package main.scala.adventofcode2020

import scala.io.Source

object Day3 {
  case class PasswordLine(min: Int, max: Int, letter: Char, password: String)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3.txt").getLines.toList
    val boolLines = processInput(lines)

    println(part1(boolLines))
    println(part2(boolLines))
  }

  def part1(lines: Array[Array[Boolean]]): Int =
    followSlope(1,3, lines)


  def part2(lines: Array[Array[Boolean]]): Long = List((1,1), (3,1),(5,1),(7,1),(1,2)).foldLeft(1L){(product, nextSlope) => {
    val (xChange, yChange) = nextSlope
    val treeCount = followSlope(yChange, xChange, lines)
    System.out.println((treeCount))
    product * followSlope(yChange, xChange, lines)
  }}

  def followSlope(yChange: Int, xChange: Int, lines: Array[Array[Boolean]]) = {
    lines.grouped(yChange).map(_(0)).foldLeft((0,0)) {
      (acc, nextLine) =>
      {
        val (totalTrees, position) = acc
        (totalTrees + {if (nextLine(position)) 1 else 0}, (position + xChange) % nextLine.length)
      }
    }._1
  }

  def processInput(lines: List[String]):Array[Array[Boolean]] =
     lines.map(line => line.toCharArray().map(x => x == '#')).toArray;

}

