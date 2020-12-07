package day07

import util.Utils.readInputLines

import scala.util.{Failure, Success}

object HandyHaversacks extends App {

	val input = readInputLines("day07/input.dat") match {
		case Success(list) => list
		case Failure(_) => List()
	}

}
