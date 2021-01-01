package main.scala.adventofcode2020

import scala.io.Source

object Day8 {
  case class Instruction(instruction: String, number: Int)
  case class MachineState(currentInstruction: Int, acc: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day8.txt").getLines.toList
    val passwordLines = processInput(lines)

    println(part1(passwordLines))
    println(part2(passwordLines))
  }

  def part1(instructions: Array[Instruction]): Int = {
    var state = MachineState(0, 0)
    var previousState: MachineState = state
    val ranInstructions = scala.collection.mutable.Set[Int]()
    while (!ranInstructions.contains(state.currentInstruction)) {
      previousState = state
      ranInstructions.add(state.currentInstruction)
      state = tick(instructions, state)
    }
    previousState.acc
  }

  def part2(lines: Array[Instruction]): Int = {
    (lines.indices)
      .map(index => {
        if (lines(index).instruction.equals("jmp")) {
          val patchedLines = lines.updated(index, Instruction("nop", -1))
          runToCompletion(patchedLines)
        } else {
          None
        }
      })
      .find(_.isDefined)
      .flatten
      .get
  }

  def runToCompletion(instructions: Array[Instruction]): Option[Int] = {
    var state = MachineState(0, 0)
    val ranInstructions = scala.collection.mutable.Set[Int]()
    while (state.currentInstruction < instructions.length && state.currentInstruction >= 0) {
      if (ranInstructions.contains(state.currentInstruction)) {
        return None
      } else {
        ranInstructions.add(state.currentInstruction)
        state = tick(instructions, state)
      }
    }
    if (state.currentInstruction == instructions.length) {
      Some(state.acc)
    } else {
      None
    }
  }

  def tick(instructions: Array[Instruction],
           machineState: MachineState): MachineState = {
    val instruction = instructions(machineState.currentInstruction)
    if (instruction.instruction.equals("jmp")) {
      machineState.copy(
        currentInstruction = machineState.currentInstruction + instruction.number
      )
    } else if (instruction.instruction.equals("acc")) {
      MachineState(
        machineState.currentInstruction + 1,
        machineState.acc + instruction.number
      )
    } else {
      machineState.copy(
        currentInstruction = machineState.currentInstruction + 1
      )
    }
  }

  def processInput(lines: List[String]): Array[Instruction] = {
    val inputLine = "(.+) (?:\\+?)(.+)".r

    lines.map {
      case inputLine(instruction, number) =>
        Instruction(instruction, number.toInt)
    }.toArray
  }

}
