package day06

import util.Utils.readInputString

import scala.util.{Failure, Success}

object CustomCustoms extends App {

	val input = readInputString("day06/input.dat") match {
		case Success(str) => str
		case Failure(_) => ""
	}

	def countAnswers: Int = {
		val groups = input split "\n\n"
		var count = 0

		for (group <- groups) {
			var yes: Set[Char] = Set()
			for (line <- group split "\n") {
				for (c <- line) {
					yes += c
				}
			}
			count += yes.size
		}
		count
	}

	println(s"count = $countAnswers")
}
