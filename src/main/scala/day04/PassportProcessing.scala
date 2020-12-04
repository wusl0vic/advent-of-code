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

	println(input)

}
