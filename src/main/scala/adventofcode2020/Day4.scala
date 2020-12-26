package main.scala.adventofcode2020

import java.util.NoSuchElementException

import scala.io.Source

object Day4 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4.txt").getLines()
    val passports = processInput(lines)

    println(part1(passports))


    val lines2 = Source.fromResource("day4.txt").getLines()
    val passports2 = processInput(lines2)
    println(part2(passports2))
  }

  def part1(lines:  Iterator[Map[String, String]] ): Int = {
    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    lines.count(passport => {
      val keys = passport.keys.toSet;
      requiredFields.forall(field => keys.contains(field))
    })
  }


  def part2(lines: Iterator[Map[String, String]]): Int =
    lines.count(passport => passportMatchesRequirements(passport))


  val yearRegex = raw"^[0-9]{4}$$".r
  val heightRegex = raw"^([0-9]+)(cm|in)$$".r
  val hairColorRegex = raw"^#[0-9a-z]{6}$$".r
  val eyeColorRegex = raw"^amb|blu|brn|gry|grn|hzl|oth$$".r
  val passportNumbRegex = raw"^[0-9]{9}$$".r
  val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  def passportMatchesRequirements(passport: Map[String, String]): Boolean = {
    val keys = passport.keys.toSet;
    if (!requiredFields.forall(field => keys.contains(field))) {
      return false
    }

    val birthYear: String = passport.get("byr").get
    if (!yearRegex.matches(birthYear)) {
      return false
    }
    if (birthYear.toInt < 1920 || birthYear.toInt > 2002) {
      return false
    }

    val issueYear: String = passport.get("iyr").get
    if (!yearRegex.matches(issueYear)) {
      return false
    }
    if (issueYear.toInt < 2010 || issueYear.toInt > 2020) {
      return false
    }

    val expYear: String = passport.get("eyr").get
    if (!yearRegex.matches(expYear)) {
      return false
    }
    if (expYear.toInt < 2020 || expYear.toInt > 2030) {
      return false
    }

    val height: String = passport.get("hgt").get
    height match {
      case heightRegex(units, dimension) => {
        val unitsInt = units.toInt
        if (dimension.equals("cm") && (unitsInt < 150 || unitsInt > 193 )) {
          return false
        }
        if (dimension.equals("in") && (unitsInt < 59 || unitsInt > 76 )) {
          return false
        }
      }
      case _ => return false
    }

    if (!hairColorRegex.matches(passport.get("hcl").get)) {
      return false
    }

    if (!eyeColorRegex.matches(passport.get("ecl").get)) {
      return false
    }

    if (!passportNumbRegex.matches(passport.get("pid").get)) {
      return false
    }

     true
  }

  def processInput(lines: Iterator[String]): Iterator[Map[String, String]] = new Iterator[Map[String, String]] {
    val passportField =  raw"([a-z]+):(\S+)".r
    def next = {
      if (!lines.hasNext) {
        throw new NoSuchElementException
      }

      var nextVal: Option[String] = None
      var resultMap = scala.collection.mutable.Map[String,String]();
      while((nextVal == None || !nextVal.get.isEmpty) && lines.hasNext) {
        nextVal = Some(lines.next())
        if (nextVal != None && !nextVal.get.isEmpty) {
          passportField.findAllIn(nextVal.get).matchData.foreach(md => resultMap.addOne((md.group(1), md.group(2))))
        }
      }
      resultMap.toMap
    }

    def hasNext = lines.hasNext
  }
}

