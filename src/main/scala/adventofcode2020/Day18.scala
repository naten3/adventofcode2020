package main.scala.adventofcode2020

import scala.io.Source

object Day18 {
  def main(args: Array[String]): Unit = {
    val lines =
      Source
        .fromResource("day18.txt")
        .getLines
        .map(_.replaceAll(" ", "").toArray)
        .toList

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[Array[Char]]): Long =
    lines
      .map(performLeftToRightArithmetic)
      .sum

  def part2(lines: List[Array[Char]]): Long =
    lines.map(performAddFirstArithmetic).sum

  def charToOperation =
    Map[Char, (Long, Long) => Long](
      '*' -> (_ * _),
      '-' -> (_ - _),
      '+' -> (_ + _)
    )

  // work from the end of the string backwards
  def performLeftToRightArithmetic(s: Array[Char]): Long = {
    if (s.length == 1) {
      return s.head.asDigit
    }

    val (rightTotal, leftRemainder): (Long, Array[Char]) = if (s.last.isDigit) {
      (s.last.asDigit, s.slice(0, s.length - 1))
    } else if (s.last == ')') {
      var openParens = 1
      var index = s.length - 2
      while (openParens > 0) {
        if (s(index) == '(') {
          openParens -= 1
        } else if (s(index) == ')') {
          openParens += 1
        }
        index -= 1
      }
      // when we exit the loop index should be one before closing parentheses
      val rightSide = performLeftToRightArithmetic(
        s.slice(index + 2, s.length - 1)
      )
      val remaining = s.slice(0, index + 1)
      (rightSide, remaining)
    } else {
      throw new RuntimeException("Can't handle " + s.head)
    }

    if (leftRemainder.isEmpty) {
      rightTotal
    } else {
      val function = charToOperation(leftRemainder.last)
      val leftTotal = performLeftToRightArithmetic(
        leftRemainder.slice(0, leftRemainder.length - 1)
      )
      function(rightTotal, leftTotal)
    }
  }

  def performAddFirstArithmetic(s: Array[Char]): Long = {
    var asteriskIndex = -1;
    var openParens = 0;
    var i = 0
    while (i < s.length && asteriskIndex < 0) {
      if (s(i) == '(') {
        openParens += 1
      } else if (s(i) == ')') { openParens -= 1 } else if (s(i) == '*' && openParens == 0) {
        asteriskIndex = i
      }
      i += 1
    }

    if (asteriskIndex > 0) {
      addSubtractString(s.slice(0, asteriskIndex)) * performAddFirstArithmetic(
        s.slice(asteriskIndex + 1, s.length)
      )
    } else {
      addSubtractString(s)
    }
  }

  // perform arithmetic on a string with no multiplication top level
  def addSubtractString(s: Array[Char]): Long = {
    if (s.length == 1) {
      s(0).asDigit
    } else {
      var positive = true
      var terms = scala.collection.mutable.ListBuffer[(Boolean, Int, Int)]()
      var openParens = 0
      var termStart = 0
      s.indices.foreach(i => {
        if (s(i) == '(') {
          openParens += 1
        } else if (s(i) == ')') {
          openParens -= 1
        } else if ((s(i) == '+' || s(i) == '-') && openParens == 0) {
          terms.append((positive, termStart, i))
          termStart = i + 1
          positive = s(i) == '+'
        }
      })
      terms.append((positive, termStart, s.length))

      terms
        .map(t => { if (t._1) 1 else -1 } * handleParens(s.slice(t._2, t._3)))
        .sum

    }
  }

  // perform arithmetic on a string with no top level addition/subtraction/multiplication
  // so only a digit or parenthese expression
  def handleParens(s: Array[Char]): Long = {
    if (s.length == 1) {
      s(0).asDigit
    } else {
      // must be parens
      performAddFirstArithmetic(s.slice(1, s.length - 1))
    }
  }

}
