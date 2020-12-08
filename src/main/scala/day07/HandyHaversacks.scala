package day07

import util.Utils.readInputLines

import scala.collection.mutable
import scala.util.{Failure, Success}

object HandyHaversacks extends App {

	val input = readInputLines("day07/input.dat") match {
		case Success(list) => list
		case Failure(_) => List()
	}

	val pattern = """^(.+)bags contain (no other bags|\d.*).$""".r
	val containP = """.*(\d)(.+) bag.*""".r

	val bagMap: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
	var isIn = Map().withDefaultValue(Set())

	def getInMap: Map[String, Set[(Int, String)]] = {
		var map: Map[String, Set[(Int, String)]] = Map()
		for (line <- input) {
			line.trim match {
				case pattern(color, indicator) =>
					if (!(indicator startsWith "no")) {
						var set: Set[(Int, String)] = Set()
						for (bag <- indicator.split(",")) {
							bag.trim match {
								case containP(nr, colour) =>
									set += Tuple2(nr.toInt, colour.trim)
								case _ => println(s"$bag did not match")
							}
						}
						map += (color.trim -> set)
					}
			}
		}
		map
	}

	def getContainedMap: Map[String, Set[String]] = {
		var map: Map[String, Set[String]] = Map()
		for (line <- input) {
			line.trim match {
				case pattern(color, indicator) =>
					if (!(indicator startsWith "no")) {
						for (bag <- indicator.split(",")) {
							bag.trim match {
								case containP(nr, colour) =>
									val set: Set[String] = map.getOrElse(colour.trim, Set())
									map += (colour.trim -> (set + color.trim))
								case _ => println(s"$bag did not match")
							}
						}
					}
			}
		}
		map
	}

	val map = getInMap
	val containedIn = getContainedMap
	var holdsShiny: Set[String] = Set()

	def check(colour: String): Unit = {
		for (bag <- containedIn.getOrElse(colour, Set())) {
			holdsShiny += bag
			check(bag)
		}
	}

	check("shiny gold")
//	println(holdsShiny.mkString(", "))
	println(holdsShiny.size)

	def cost(colour: String): Int = {
		if (map.contains(colour)) {
			var c = 0
			for (col <- map(colour)) {
				c += col._1 + col._1 * cost(col._2)
			}
			c
		} else{
			0
		}
	}

	val dollarz = cost("shiny gold")
	println(s"cost = $dollarz")
}