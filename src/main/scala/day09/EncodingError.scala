package day09

import util.Utils.readInputLines

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

	val x = findWeakness(input, 25)
	println(x)

}
