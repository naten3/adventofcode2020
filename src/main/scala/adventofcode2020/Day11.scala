package main.scala.adventofcode2020

import main.scala.adventofcode2020.Day11.SeatState.SeatState

import scala.io.Source

object Day11 {

  object SeatState extends Enumeration {
    type SeatState = Value
    val Empty, Occupied, Floor = Value
  }

  def main(args: Array[String]): Unit = {
    val lines =
      Source
        .fromResource("day11.txt")
        .getLines
        .map(
          _.toCharArray.map(
            char =>
              if (char == 'L') SeatState.Empty
              else if (char == '#') SeatState.Occupied
              else SeatState.Floor
          )
        )
        .toArray

    println(part1(lines))
    println(part2(lines))
  }

  def part1(lines: Array[Array[SeatState]]): Int =
    getStableStateCount(lines, getNewState)

  def part2(lines: Array[Array[SeatState]]): Int =
    getStableStateCount(lines, getNewStateDirectionSearchFunction)

  def getStableStateCount(
    lines: Array[Array[SeatState]],
    newStateFunction: (Array[Array[SeatState]]) => (Int, Int) => NewStateResult
  ): Int = {
    var (seats, changed) = (lines, true)
    while (changed) {
      seats.foreach(
        row =>
          System.out.println(
            row.map(
              x =>
                if (x == SeatState.Occupied) '#'
                else if (x == SeatState.Floor) '.'
                else 'L'
            )
        )
      )
      System.out.println()
      val incrementResult = increment(seats, newStateFunction)
      seats = incrementResult.newState
      changed = incrementResult.changed
    }

    seats.map(row => row.count(x => x == SeatState.Occupied)).sum
  }

  case class IncrementResult(newState: Array[Array[SeatState]],
                             changed: Boolean)
  def increment(
    seats: Array[Array[SeatState]],
    newStateFn: (Array[Array[SeatState]]) => (Int, Int) => NewStateResult
  ): IncrementResult = {

    val newStateFunction = newStateFn(seats)

    val (newSeats, anySeatChanged) =
      seats.indices.foldLeft((new Array[Array[SeatState]](0), false)) {
        (acc, row) =>
          val (newRows: Array[Array[SeatState]], changed) = acc
          val oldRow = seats(row)
          val (newRow: Array[SeatState], rowIsDifferent) =
            oldRow.indices.foldLeft((new Array[SeatState](0), false)) {
              (colAcc, col) =>
                val (currentRow, rowChanged) = colAcc
                val NewStateResult(newItem, itemChanged) =
                  newStateFunction(row, col)
                (currentRow :+ newItem, rowChanged || itemChanged)
            }

          val newRowAr: Array[Array[SeatState]] = newRows :+ newRow
          (newRowAr, changed || rowIsDifferent)
      }

    IncrementResult(newSeats, anySeatChanged)
  }

  case class NewStateResult(state: SeatState, changed: Boolean)
  def getNewState(seats: Array[Array[SeatState]])(row: Int,
                                                  col: Int): NewStateResult = {
    val oldState = seats(row)(col)
    if (oldState == SeatState.Floor) {
      return NewStateResult(SeatState.Floor, false)
    }

    val occupiedNeighbors = List(
      (row - 1, col - 1),
      (row - 1, col),
      (row - 1, col + 1),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col - 1),
      (row + 1, col),
      (row + 1, col + 1),
    ).map((coords) => {
        val (row, col) = coords
        if (row >= 0 && row < seats.length && col >= 0 & col < seats(0).length) {
          Some(seats(row)(col))
        } else {
          None
        }
      })
      .count(
        neighbor => neighbor.isDefined && neighbor.get == SeatState.Occupied
      )

    val newState = if (occupiedNeighbors == 0) {
      SeatState.Occupied
    } else if (occupiedNeighbors >= 4) {
      SeatState.Empty
    } else {
      oldState
    }
    NewStateResult(newState, newState != oldState)
  }

  def getNewStateDirectionSearchFunction(
    seats: Array[Array[SeatState]]
  ): (Int, Int) => NewStateResult = {
    // save only the min and max occupied for rows, columns, and both diagonals in a map
    case class MinMax(minIndex: Int, maxIndex: Int)
    val rowMinMax = scala.collection.mutable.Map[Int, MinMax]()
    val colMinMax = scala.collection.mutable.Map[Int, MinMax]()
    // for diagonals use  sum or diff of indices as key, use column as min max value
    val diagSumMinMax = scala.collection.mutable.Map[Int, MinMax]()
    val diagDiffMinMax = scala.collection.mutable.Map[Int, MinMax]()

    def updateMinMaxVal(map: scala.collection.mutable.Map[Int, MinMax],
                        key: Int,
                        value: Int): Unit = {

      val existingMinMax = map.get(key)
      if (existingMinMax.isEmpty) {
        map.put(key, MinMax(value, value))
      } else {
        map.put(
          key,
          MinMax(
            Math.min(existingMinMax.get.minIndex, value),
            Math.max(existingMinMax.get.maxIndex, value)
          )
        )
      }
    }

    seats.indices.foreach(
      row =>
        seats(row).indices.foreach(col => {
          if (seats(row)(col) == SeatState.Occupied) {
            updateMinMaxVal(rowMinMax, row, col)
            updateMinMaxVal(colMinMax, col, row)
            updateMinMaxVal(diagSumMinMax, col + row, col)
            updateMinMaxVal(diagDiffMinMax, col - row, col)
          }
        })
    )

    def getNewStateDirectionSearch(row: Int, col: Int): NewStateResult = {
      val oldState = seats(row)(col)
      if (oldState == SeatState.Floor) {
        return NewStateResult(SeatState.Floor, false)
      }

      def findNeighbor(
        row: Int,
        col: Int,
        rowColIncrementFn: (Int, Int) => (Int, Int)
      ): Option[SeatState] = {
        var currentRow = row
        var currentCol = col
        var hasIncremented = false
        while (currentRow >= 0 && currentRow < seats.length && currentCol >= 0 && currentCol < seats(
                 0
               ).length) {
          if (hasIncremented) {
            val currentSeat = seats(currentRow)(currentCol)
            if (currentSeat != SeatState.Floor) {
              return Some(currentSeat)
            }
          }
          val incrementResult = rowColIncrementFn(currentRow, currentCol)
          currentRow = incrementResult._1
          currentCol = incrementResult._2
          hasIncremented = true
        }
        None
      }

      val occupiedCount = List[(Int, Int) => (Int, Int)](
        (r, c) => (r, c + 1),
        (r, c) => (r, c - 1),
        (r, c) => (r + 1, c),
        (r, c) => (r + 1, c + 1),
        (r, c) => (r + 1, c - 1),
        (r, c) => (r - 1, c),
        (r, c) => (r - 1, c + 1),
        (r, c) => (r - 1, c - 1)
      ).map(findNeighbor(row, col, _))
        .count(_.exists(x => x == SeatState.Occupied))

      val newState = if (occupiedCount == 0) {
        SeatState.Occupied
      } else if (occupiedCount >= 5) {
        SeatState.Empty
      } else {
        oldState
      }
      NewStateResult(newState, newState != oldState)
    }

    getNewStateDirectionSearch
  }

}
