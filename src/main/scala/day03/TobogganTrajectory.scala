package day03

import util.Utils.readInput

import scala.Console.{BLUE, RESET, YELLOW}
import scala.util.{Failure, Success}

object TobogganTrajectory extends App {

	val input: List[String] = readInput("day03/input.dat") match {
		case Success(list) => list
		case Failure(_) => List()
	}

	def rideTheSlide(track: List[String], right: Int, down: Int): Int = {
		var i = 0
		var trees = 0
		for (j <- down until track.length by down) {
			i = (i + right) % track(j).length
			if (track(j)(i) == '#') {
				trees += 1
			}
		}
		trees
	}

	val trees = rideTheSlide(input, 3, 1)
	println(s"We crashed into$BLUE $trees$RESET trees")

	val t_one   = rideTheSlide(input, 1, 1)
	val t_two   = rideTheSlide(input, 3, 1)
	val t_three = rideTheSlide(input, 5, 1)
	val t_four  = rideTheSlide(input, 7, 1)
	val t_five  = rideTheSlide(input, 1, 2)

	println(s"$t_one, $t_two, $t_three, $t_four, $t_five")
	println(s"prod = $YELLOW${t_one * t_two * t_three * t_four * t_five}$RESET")

}
