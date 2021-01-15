package main.scala.adventofcode2020

import main.scala.adventofcode2020.Day12.Direction.Direction

import scala.io.Source

object Day12 {
  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West, Forward, Left, Right = Value
  }

  val numToDirectionMap = Map(
    0 -> Direction.North,
    1 -> Direction.East,
    2 -> Direction.South,
    3 -> Direction.West
  )
  val directionToNumMap = numToDirectionMap.map(_.swap)

  case class Instruction(instruction: Direction, number: Int)
  case class State(x: Int, y: Int, currentDirection: Direction)
  case class State2(shipX: Int,
                    shipY: Int,
                    waypointX: Int,
                    waypointY: Int,
                    waypointDirection: Direction)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day12.txt").getLines.toList
    val directions = processInput(lines)

    println(part1(directions))
    println(part2(directions))
  }

  def part1(instructions: Array[Instruction]): Int = {
    val finalPosition = instructions.foldLeft(State(0, 0, Direction.East))(move)
    manhattanDistance(finalPosition.x, finalPosition.y)
  }

  def part2(instructions: Array[Instruction]): Int = {
    val finalPosition =
      instructions.foldLeft(State2(0, 0, 10, 1, Direction.East))(move2)
    manhattanDistance(finalPosition.shipX, finalPosition.shipY)
  }

  def manhattanDistance(x: Int, y: Int) = Math.abs(x) + Math.abs(y)

  def move(prevState: State, instruction: Instruction): State =
    instruction.instruction match {
      case Direction.East =>
        prevState.copy(x = prevState.x + instruction.number)
      case Direction.West =>
        prevState.copy(x = prevState.x - instruction.number)
      case Direction.North =>
        prevState.copy(y = prevState.y + instruction.number)
      case Direction.South =>
        prevState.copy(y = prevState.y - instruction.number)
      case Direction.Right => {
        val directionNum = directionToNumMap(prevState.currentDirection)
        val newDirectionNum =
          Math.floorMod(directionNum + instruction.number / 90, 4)
        val newDirection = numToDirectionMap(newDirectionNum)
        prevState.copy(currentDirection = newDirection)
      }
      case Direction.Left => {
        val directionNum = directionToNumMap(prevState.currentDirection)
        val newDirectionNum =
          Math.floorMod(directionNum - instruction.number / 90, 4)
        val newDirection = numToDirectionMap(newDirectionNum)
        prevState.copy(currentDirection = newDirection)
      }
      case Direction.Forward =>
        move(
          prevState,
          Instruction(prevState.currentDirection, instruction.number)
        )
    }

  def rotateWaypoint(prevState: State2, instruction: Instruction): State2 = {
    // how many clockwise 90 degree rotations is this
    val rotateClockwiseTimes = instruction.instruction match {
      case Direction.Left  => Math.floorMod(-1 * instruction.number / 90, 4)
      case Direction.Right => instruction.number / 90
    }

    rotateClockwiseTimes match {
      case 1 =>
        prevState.copy(
          waypointX = prevState.waypointY,
          waypointY = -1 * prevState.waypointX
        )
      case 2 =>
        prevState.copy(
          waypointX = -1 * prevState.waypointX,
          waypointY = -1 * prevState.waypointY
        )
      case 3 =>
        prevState.copy(
          waypointX = -1 * prevState.waypointY,
          waypointY = prevState.waypointX
        )
    }
  }

  def waypointMove(prevState: State2, waypointMultiplier: Int): State2 =
    prevState.copy(
      shipX = prevState.shipX + prevState.waypointX * waypointMultiplier,
      shipY = prevState.shipY + prevState.waypointY * waypointMultiplier
    )

  def move2(prevState: State2, instruction: Instruction): State2 =
    instruction.instruction match {
      case Direction.East =>
        prevState.copy(waypointX = prevState.waypointX + instruction.number)
      case Direction.West =>
        prevState.copy(waypointX = prevState.waypointX - instruction.number)
      case Direction.North =>
        prevState.copy(waypointY = prevState.waypointY + instruction.number)
      case Direction.South =>
        prevState.copy(waypointY = prevState.waypointY - instruction.number)
      case Direction.Right   => rotateWaypoint(prevState, instruction)
      case Direction.Left    => rotateWaypoint(prevState, instruction)
      case Direction.Forward => waypointMove(prevState, instruction.number)
    }

  def processInput(lines: List[String]): Array[Instruction] = {
    lines.map { line =>
      val directionLeter = line.charAt(0)
      val number = line.substring(1).toInt
      val direction = directionLeter match {
        case 'N' => Direction.North
        case 'S' => Direction.South
        case 'E' => Direction.East
        case 'W' => Direction.West
        case 'R' => Direction.Right
        case 'L' => Direction.Left
        case 'F' => Direction.Forward
      }
      Instruction(direction, number)
    }.toArray
  }

}
