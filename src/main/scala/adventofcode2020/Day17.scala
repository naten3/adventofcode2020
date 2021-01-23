package main.scala.adventofcode2020

import scala.io.Source

object Day17 {
  type InputArray = Array[Array[Boolean]]
  type Coordinate = List[Int]

  val inputDimensions = 2;
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("day17.txt")
      .getLines
      .map(_.map(_ == '#').toArray)
      .toArray

    println(part1(input))
    println(part2(input))
  }

  def part1(input: InputArray): Int = {
    iterateCubesOverDimension(3, input)
  }

  def part2(input: InputArray): Int = {
    iterateCubesOverDimension(4, input)
  }

  def iterateCubesOverDimension(dimensions: Int, input: InputArray) = {
    val startCoordinates = getStartCoordinates(input, dimensions)

    val onCoordinatesAfterCycles = (1 to 6).foldLeft(startCoordinates) {
      (onCoordinates, _) =>
        iterateCubes(onCoordinates, dimensions)
    }

    getOnCoordinates(onCoordinatesAfterCycles, dimensions)
  }

  def getOnCoordinates(coordinates: Set[Coordinate], dimensions: Int): Int = {
    coordinates.toSeq
      .map(getCoordinateCountAfterSymmetry(_, dimensions))
      .sum
  }

  def getCoordinateCountAfterSymmetry(c: Coordinate, dimensions: Int): Int = {
    val returnValue =
      (inputDimensions + 1 to dimensions).foldLeft(1)((product, nextDim) => {
        // handle symmetry by multiplying by 2 if the dimension isn't 0
        // all dimensions after input dimension must be symmetrical
        if (c(nextDim - 1) != 0) {
          product * 2
        } else {
          product
        }
      })
    returnValue
  }

  def getStartCoordinates(lines: InputArray,
                          dimensions: Int): Set[Coordinate] = {
    lines.zipWithIndex
      .flatMap(
        lineWithIndex =>
          lineWithIndex._1.zipWithIndex
            .filter(elemWithIndex => elemWithIndex._1)
            .map(
              elemWithIndex =>
                Array(elemWithIndex._2, lineWithIndex._2)
                  .concat(((inputDimensions + 1) to dimensions).map(_ => 0))
          )
      )
      .map(_.toList)
      .toSet
  }

  // Should the cube be on next cycle
  def isOn(onNeighborCount: Int, isCurrentlyOn: Boolean, coord: Coordinate) = {
    if (isCurrentlyOn) {
      onNeighborCount == 2 || onNeighborCount == 3
    } else {
      onNeighborCount == 3
    }
  }

  def iterateCubes(onCoordinates: Set[Coordinate],
                   dimensions: Int): Set[Coordinate] = {

    onCoordinates
      .flatMap(coord => coord +: getNeighbors(coord, dimensions))
      .filter(
        coord =>
          isOn(
            onNeighborCount(coord, onCoordinates, dimensions),
            onCoordinates.contains(coord),
            coord
        )
      )
  }

  def onNeighborCount(coord: Coordinate,
                      coordinates: Set[Coordinate],
                      dimensions: Int): Int =
    getNeighbors(coord, dimensions).count(coordinates.contains)

  def getNeighbors(coord: Coordinate, dimensions: Int): List[Coordinate] = {

    // Get the range of coordinates along a single dimension
    def getNeighborCoords(dimension: Int): Array[Int] = {
      val i = coord(dimension - 1)
      // take advantage of symmetry
      if (i == 0 && dimension > inputDimensions) {
        Array(i + 1, i, i + 1)
      } else {
        Array(i - 1, i, i + 1)
      }
    }

    (1 to dimensions)
      .foldLeft(Array[Coordinate]())((ndCoordinates, dimension) => {
        if (ndCoordinates.isEmpty) {
          getNeighborCoords(dimension).map(List(_))
        } else {
          ndCoordinates.flatMap(
            partialNeighborCoord =>
              getNeighborCoords(dimension)
                .map(neighborCoord => partialNeighborCoord :+ neighborCoord)
          )
        }
      })
      .filter(_ != coord)
      .toList
  }
}
