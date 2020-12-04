package day03

import util.Utils.readInputLines

import scala.Console.{BLUE, RESET, YELLOW}
import scala.util.{Failure, Success}

object TobogganTrajectory extends App {

	implicit val input: List[String] = readInputLines("day03/input.dat") match {
		case Success(list) => list
		case Failure(_) => List()
	}

	def rideTheSlide(right: Int, down: Int)(implicit track: List[String]): Int = {
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

	val trees = rideTheSlide(3, 1)
	println(s"We crashed into$BLUE $trees$RESET trees")

	val t_one   = rideTheSlide(1, 1)
	val t_two   = rideTheSlide(3, 1)
	val t_three = rideTheSlide(5, 1)
	val t_four  = rideTheSlide(7, 1)
	val t_five  = rideTheSlide(1, 2)

	println(s"$t_one, $t_two, $t_three, $t_four, $t_five")
	println(s"prod = $YELLOW${t_one * t_two * t_three * t_four * t_five}$RESET")

}
