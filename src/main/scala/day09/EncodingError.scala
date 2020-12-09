package day09

import util.Utils.readInputLines

import scala.Console.{RED, RESET, YELLOW}
import scala.util.{Failure, Success}

object EncodingError extends App {

	val input = readInputLines("day09/input.dat") match {
		case Success(l) => l.map(_.toLong)
		case Failure(_) => List()
	}

	val testInput: List[Long] = List(35,
	20,
	15,
	25,
	47,
	40,
	62,
	55,
	65,
	95,
	102,
	117,
	150,
	182,
	127,
	219,
	299,
	277,
	309,
	576)

	def findWeakness(input: List[Long], preamble: Int): Long = {
		for (i <- preamble until input.length) {
			if (!hasSum(input.slice(i - preamble, i), input(i)))
				return input(i)
		}
		def hasSum(list: List[Long], exp: Long): Boolean = {
			for (i <- 0 until list.length - 1) {
				for (j <- i + 1 until list.length) {
					if (list(i) + list(j) == exp) return true
				}
			}
			false
		}
		-1
	}

	val weakSpot = findWeakness(input, 25)
	println(s"the weakness in the XMAS data is $RED$weakSpot$RESET")

	def findSubList(input: List[Long], exp: Long): List[Long] = {
		for (i <- 0 until input.length - 1) {
			var sum = input(i)
			for (j <- i + 1 until input.length) {
				sum += input(j)
				if (sum == exp)
					return input.slice(i, j + 1)
			}
		}
		List()
	}

	val sublist = findSubList(input, weakSpot)
	println(s"resolving weakness: ${sublist.min} + ${sublist.max} = $YELLOW${sublist.min + sublist.max}$RESET")

}
