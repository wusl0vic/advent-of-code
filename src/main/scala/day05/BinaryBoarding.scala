package day05

import util.Utils.readInputLines

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object BinaryBoarding extends App {

	val input = readInputLines("day05/input.dat") match {
		case Success(l) => l
		case Failure(_) => List()
	}
	val rowRange = 0 until 128
	val colRange = 0 until 8

	def getRowNr(code: String): Int = {
		getIndex(code.substring(0, 7), rowRange)
	}

	def getColNr(code: String): Int = {
		getIndex(code takeRight 3, colRange)
	}

	@tailrec
	def getIndex(code: String, range: Range): Int = {
		def getRange(c: Char, range: Range): Range = {
			c match {
				case 'F' | 'L' => range.start until middle(range) + 1
				case 'B' | 'R' => middle(range) + 1 until range.last + 1
				case _ => -1 until -1
			}
		}
		if (code.isEmpty)
			return range.start
		val r = getRange(code(0), range)
		code match {
			case "" => r.start
			case _ => getIndex(code.takeRight(code.length - 1), r)
		}
	}

	def getSeatNr(code: String): (Int, Int) = {
		(getRowNr(code), getColNr(code))
	}

	def getSeatId(seat: (Int, Int)): Int = {
		seat._1 * 8 + seat._2
	}

	def middle(range: Range): Int = {
		if (range.isEmpty)
			return 0
		range.start + (range.last - range.start) / 2
	}

	def getSeatIds: Set[Int] = {
		@tailrec
		def getSeatIds(list: List[String], acc: Set[Int]): Set[Int] = list match {
			case Nil => acc
			case x :: xs => getSeatIds(xs, acc ++ Set(getSeatId(getSeatNr(x))))
		}
		getSeatIds(input, Set())
	}

	val max = getSeatIds.max
	println(s"max = $max")

	val sorted: Seq[Int] = getSeatIds.toSeq.sorted
	val range: Set[Int] = (sorted.head until sorted.last).toSet
	val mine: Set[Int] = range -- sorted.toSet
	println(mine.mkString(","))


}
