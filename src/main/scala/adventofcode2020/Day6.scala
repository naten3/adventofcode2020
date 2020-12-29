package main.scala.adventofcode2020

import java.util.NoSuchElementException

import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day6.txt").getLines()
    val groups = processInput(lines)

    println(part1(groups))


    val lines2 = Source.fromResource("day6.txt").getLines()
    val groups2 = processInput(lines2)
    println(part2(groups2))
  }

  def part1(lines: Iterator[List[String]]): Int = lines.map(group => {
    group.foldLeft(Set[Char]())((yesAnswers, personYesAnswers) => yesAnswers ++ personYesAnswers.toCharArray).size
  }).sum


  def part2(lines: Iterator[List[String]]): Int = lines.map(group => {
    group.zipWithIndex.foldLeft(Set[Char]())((yesAnswers, personYesAnswersIndex) => {
      val (personYesAnswers, index) = personYesAnswersIndex
      if (index == 0) personYesAnswers.toCharArray.toSet
      else personYesAnswers.toCharArray.toSet.intersect(yesAnswers)}).size
  }).sum

  def processInput(lines: Iterator[String]): Iterator[List[String]] = new Iterator[List[String]] {
    def next = {
      if (!lines.hasNext) {
        throw new NoSuchElementException
      }

      var nextVal: Option[String] = None
      val result = scala.collection.mutable.ListBuffer[String]();
      while((nextVal == None || !nextVal.get.isEmpty) && lines.hasNext) {
        nextVal = Some(lines.next())
        if (nextVal != None && !nextVal.get.isEmpty) {
          result.append(nextVal.get)
        }
      }
      result.toList
    }

    def hasNext = lines.hasNext
  }
}

