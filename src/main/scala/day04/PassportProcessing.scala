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
	val required: Map[String, String => Boolean] = Map(
		"byr" -> (s => s.length == 4 && s.toInt >= 1920 && s.toInt <= 2002),
		"iyr" -> (s => s.length == 4 && s.toInt >= 2010 && s.toInt <= 2020),
		"eyr" -> (s => s.length == 4 && s.toInt >= 2020 && s.toInt <= 2030),
		"hgt" -> checkHeight,
		"hcl" -> (s => """#[0-9a-f]{6}""".r matches s),
		"ecl" -> (s => s.length == 3 && Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")(s)),
		"pid" -> (s => """[0-9]{9}""".r matches s)
//		"cid" -> (_ => true)
	)

	val heightPattern = """(\d+)(cm|in)""".r
	def checkHeight(h: String): Boolean = h match {
		case heightPattern(height, unit) =>
			if (unit == "cm") height.toInt >= 150 && height.toInt <= 193
			else if (unit == "in") height.toInt >= 59 && height.toInt <= 76
			else false
		case _ => false
	}

	def countValidFields(required: Set[String])(implicit input: List[String]): Int = {
		var valid = 0
		for (line <- input) {
			val key = line.split(" ").map(_.trim).toList.map(s => s.split(":")(0)).toSet
			if (key - "cid" == required)
				valid += 1
		}
		valid
	}

	def countActuallyValidFields(check: Map[String, String => Boolean])(implicit input: List[String]): Int = {
		var valid = 0
		for (passport <- input) {
			val keys = passport.split(" ").map(_.trim).toList.map(s => s.split(":")(0)).toSet
			var success = check.keySet == keys - "cid"
			if (success) {
				for (lst <- passport.split(" ").map(_.trim).toList) {
					if (success) {
						val kv = lst.split(":").map(_.trim).toList
						if (kv.head != "cid") {
							val ck: Option[String => Boolean] = check get kv.head
							success = ck match {
								case Some(test) => test(kv(1))
								case None => false
							}
						}
					}
				}
			}
			if (success) valid += 1
		}
		valid
	}

	val valid = countValidFields(requiredFields - "cid")
	println(s"there are $valid valid fields")

	val actuallyValid = countActuallyValidFields(required)
	println(s"there are $actuallyValid actually valid fields")
}
