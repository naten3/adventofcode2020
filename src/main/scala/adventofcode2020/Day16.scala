package main.scala.adventofcode2020

import scala.io.Source

object Day16 {
  case class Range(min: Int, max: Int)
  case class Field(name: String, ranges: List[Range])
  type Ticket = Array[Int]
  case class PuzzleInput(fields: List[Field],
                         yourTicket: Ticket,
                         nearbyTickets: List[Ticket])
  def main(args: Array[String]): Unit = {
    val input = processInput(
      Source
        .fromResource("day16.txt")
        .getLines
    )

    println(part1(input))
    println(part2(input))
  }

  def part1(input: PuzzleInput): Int = {
    val ranges = input.fields.flatMap(_.ranges)
    input.nearbyTickets
      .flatMap(_.filter(ticketValue => doesNotMatchRange(ranges, ticketValue)))
      .sum
  }

  def part2(input: PuzzleInput): Long = {
    val validTickets = getValidTickets(input) :+ input.yourTicket
    val fields = input.fields

    val possibleFieldsByIndex = validTickets.head.indices
      .map(index => {
        fields
          .filter(
            field =>
              validTickets
                .forall(ticket => isValidForField(ticket(index), field))
          )
          .toSet
      })
      .toArray

    val orderedIndexList =
      possibleFieldsByIndex.zipWithIndex.sortBy(_._1.size).map(_._2).toList

    val remainingFields = scala.collection.mutable.Set[Field]()
    remainingFields.addAll(input.fields)

    def findValidTicketSequence(
      remainingIndices: List[Int]
    ): Option[List[(Field, Int)]] = {
      if (remainingIndices.isEmpty) {
        Some(List())
      } else {
        // try to pick field that goes in this index
        val nextIndex = remainingIndices.head

        possibleFieldsByIndex(nextIndex)
          .filter(field => remainingFields.contains(field))
          .map(field => {
            remainingFields.remove(field)
            val result = findValidTicketSequence(remainingIndices.tail)
              .map((field, nextIndex) +: _)
            remainingFields.add(field)
            result
          })
          .find(_.isDefined)
          .flatten
      }
    }

    val validSequence = findValidTicketSequence(orderedIndexList).get

    validSequence
      .filter(_._1.name.contains("departure"))
      .map(fieldWithIndex => {
        input.yourTicket(fieldWithIndex._2).toLong
      })
      .product
  }

  def isValidForField(value: Int, field: Field): Boolean =
    field.ranges.exists(range => value >= range.min && value <= range.max)

  def doesNotMatchRange(ranges: List[Range], ticketValue: Int): Boolean =
    !ranges.exists(
      range => ticketValue >= range.min && ticketValue <= range.max
    )

  def getValidTickets(input: PuzzleInput): List[Ticket] = {
    val ranges = input.fields.flatMap(_.ranges)
    input.nearbyTickets.filter(
      ticketValues =>
        !ticketValues
          .exists(ticketValue => doesNotMatchRange(ranges, ticketValue))
    )
  }

  def processInput(lines: Iterator[String]): PuzzleInput = {
    val fieldReg = "(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
    val fields = scala.collection.mutable.ListBuffer[Field]()
    var line = lines.next
    while (!line.isEmpty) {
      line match {

        case fieldReg(name, r1s, r1e, r2s, r2e) =>
          fields.addOne(
            Field(
              name,
              List(Range(r1s.toInt, r1e.toInt), Range(r2s.toInt, r2e.toInt))
            )
          )
      }
      line = lines.next
    }

    lines.next()
    line = lines.next
    val yourTicket = parseTicket(line)
    lines.next
    lines.next

    val nearbyTickets = scala.collection.mutable.ListBuffer[Ticket]()
    while (lines.hasNext) {
      line = lines.next
      nearbyTickets.addOne(parseTicket(line))
    }

    PuzzleInput(fields.toList, yourTicket, nearbyTickets.toList)
  }

  def parseTicket(input: String): Ticket = input.split(",").map(_.toInt).toArray

}
