package main.scala.adventofcode2020

import scala.io.Source

object Day14 {
  abstract class InputLine
  case class Mask(mask: String) extends InputLine
  case class WriteInstruction(memory: Int, value: Long) extends InputLine

  def main(args: Array[String]): Unit = {

    val lines = processInput(Source.fromResource("day14.txt").getLines)

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: List[InputLine]): Long = {
    case class State(orMask: Long, andMask: Long, memory: Map[Int, Long])
    val finalState = lines.foldLeft(State(0L, 1L, Map())) { (state, next) =>
      next match {
        case Mask(mask) => {
          val andMask = java.lang.Long
            .parseLong(mask.map(c => if (c == '0') '0' else '1'), 2)
          val orMask = java.lang.Long
            .parseLong(mask.map(c => if (c == '1') '1' else '0'), 2)
          state.copy(andMask = andMask, orMask = orMask)
        }
        case WriteInstruction(memoryLocation, value) => {
          val newMemory = state.memory + (memoryLocation -> (value & state.andMask | state.orMask))
          state.copy(memory = newMemory)
        }
      }
    }
    finalState.memory.values.sum
  }

  def getMaskAddresses(mask: String, address: Int): List[String] = {
    val binaryCharAddress =
      address.toBinaryString.reverse.padTo(36, '0').reverse
    def getMaskAddresses(buffer: List[Char],
                         remaining: List[Char],
                         index: Int): List[List[Char]] = {
      if (remaining.isEmpty) {
        List(buffer)
      } else if (remaining.head == 'X') {
        List('0', '1').flatMap(
          num => getMaskAddresses(num +: buffer, remaining.tail, index + 1)
        )
      } else if (remaining.head == '1') {
        getMaskAddresses('1' +: buffer, remaining.tail, index + 1)
      } else {
        getMaskAddresses(
          binaryCharAddress(index) +: buffer,
          remaining.tail,
          index + 1
        )
      }
    }

    getMaskAddresses(List(), mask.toCharArray.toList, 0).map(_.reverse.mkString)
  }

  def part2(lines: List[InputLine]): Long = {
    case class State(mask: String, memory: Map[String, Long])
    val finalState = lines.foldLeft(State("", Map())) { (state, next) =>
      next match {
        case Mask(mask) => {
          state.copy(mask = mask)
        }
        case WriteInstruction(memoryLocation, value) => {
          val newMemory = state.memory ++ getMaskAddresses(
            state.mask,
            memoryLocation
          ).map((_, value))
          state.copy(memory = newMemory)
        }
      }
    }
    finalState.memory.values.sum
  }

  def processInput(lines: Iterator[String]): List[InputLine] = {
    val maskReg = "mask = (.*)".r
    val memReg = "mem\\[([0-9]+)\\] = ([0-9]+)".r

    lines
      .map(_ match {
        case maskReg(mask) => Mask(mask)
        case memReg(memory, value) =>
          WriteInstruction(memory.toInt, value.toLong)
      })
      .toList
  }

}
