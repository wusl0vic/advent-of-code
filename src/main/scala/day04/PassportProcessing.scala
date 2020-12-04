package day04

import util.Utils.readInputLines

import scala.util.{Failure, Success}

object PassportProcessing extends App {

	implicit val input: List[String] = readInputLines("day04/input.dat") match {
		case Success(list) =>
			var str = ""
			for (line <- list) {
				if (line != "") {
					str += " " + line
				} else {
					str += "\n"
				}
			}
			str.split("\n").map(_.trim).toList
		case Failure(_) => List()
	}

	val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")

	def countValidFields(required: Set[String])(implicit input: List[String]): Int = {
		var valid = 0
		for (line <- input) {
			val key = line.split(" ").map(_.trim).toList.map(s => s.split(":")(0)).toSet
			if (key - "cid" == required)
				valid += 1
		}
		valid
	}

	val valid = countValidFields(requiredFields - "cid")
	println(s"there are $valid valid fields")
}
