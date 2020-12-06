package day06

import util.Utils.readInputString

import scala.collection.mutable
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

	def countAllYesAnswers: Int = {
		val groups = input split "\n\n"
		var count = 0

		for (group <- groups) {
			val lines: Seq[String] = group split "\n"
			val groupMap: mutable.Map[Int, Set[Char]] = collection.mutable.Map[Int, Set[Char]]()
			for (i <- lines.indices) {
				groupMap(i) = lines(i).toCharArray.toSet
			}
			count += groupMap.values.reduce((a, b) => a intersect b).size
		}
		count
	}

	println(s"count = $countAnswers")
	println(s"count all yes = $countAllYesAnswers")
}
