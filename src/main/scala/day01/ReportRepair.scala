package day01

import util.Utils.readInputLines

import scala.util.{Failure, Success}

object ReportRepair extends App {

	val fileName = "day01/input.dat"
	implicit val input: List[Int] = readInputLines(fileName) match {
		case Success(list) => list.map(_.toInt)
		case Failure(_) => List()
	}

	def find2020(implicit list: List[Int]): (Int, Int) = {
		for (i <- 0 until list.length - 1) {
			for (j <- i + 1 until list.length) {
				val sum = list(i) + list(j)
				if (sum == 2020) {
					return (list(i),  list(j))
				}
			}
		}
		(-1, -1)
	}

	def find2020Triple(implicit list: List[Int]): (Int, Int, Int) = {
		for (i <- 0 until list.length - 2) {
			for (j <- i + 1 until list.length - 1) {
				for (k <- j + 1 until list.length) {
					val sum = list(i) + list(j) + list(k)
					if (sum == 2020) {
						return (list(i), list(j), list(k))
					}
				}
			}
		}
		(-1, -1, -1)
	}

	val result = find2020

	println(s"${result._1} + ${result._2} = ${result._1 + result._2}")
	println(s"${result._1} * ${result._2} = ${result._1 * result._2}")

	val result3 = find2020Triple
	println

	println(s"${result3._1} + ${result3._2} + ${result3._3} = ${result3._1 + result3._2 + result3._3}")
	println(s"${result3._1} * ${result3._2} * ${result3._3} = ${result3._1 * result3._2 * result3._3}")

}
