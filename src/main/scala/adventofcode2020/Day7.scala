package main.scala.adventofcode2020

import scala.io.Source

object Day7 {
  case class BagCount(bagType: String, count: Int)
  case class ColorBasketLine(bagType: String, contains: List[BagCount] )

  val goldBag = "shiny gold"

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day7.txt").getLines.toList
    val passwordLines = processInput(lines)

    println(part1(passwordLines))
    println(part2(passwordLines))
  }

  def part1(lines: List[ColorBasketLine]): Int = {

    val canHaveGoldMap = scala.collection.mutable.Map[String, Boolean]();
    val contains = lines.map((l => (l.bagType, l.contains))).toMap

    def canHaveGold(bagType: String): Boolean = {
      // for now say goldBag can contain itself, remove it from out answer
      if (bagType.equals(goldBag)) {
        true
      } else {
        canHaveGoldMap.getOrElseUpdate(bagType, contains(bagType).exists(subType => {
          canHaveGold(subType.bagType)
        }))
      }
    }

    lines.count(l => canHaveGold(l.bagType)) - 1
  }

  def part2(lines: List[ColorBasketLine]): Long = {
    val bagsInsideMap = scala.collection.mutable.Map[String, Long]();
    val canContainByType = lines.map((l => (l.bagType, l.contains))).toMap

    def bagsInside(bagType: String): Long = {
      bagsInsideMap.getOrElseUpdate(bagType, canContainByType(bagType).map(bagCount =>
        (bagCount.count * ( 1 + bagsInside(bagCount.bagType)))).sum )
    }
    bagsInside(goldBag)
  }

  def processInput(lines: List[String]): List[ColorBasketLine] = {
    val inputLine = raw"(.+) bags contain (.+).".r
    val bagMatchReg = "(?:\\s?)([0-9]+) (.+) bag(?:s?)".r
    val noContentsString = "no other bags";
    lines.map {
      case inputLine(bagType, remainder) =>
        val bagCounts: List[BagCount] = {
          if (remainder.equals(noContentsString)) {
            List()
          } else {
            remainder.split(',').map {
              case bagMatchReg(count, bagType) => BagCount(bagType, count.toInt)
            }.toList
          }
        }
        ColorBasketLine(bagType, bagCounts)
    }
  }

}

