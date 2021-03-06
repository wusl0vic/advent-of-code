package day02

import util.Utils.readInputLines

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object PasswordPhilosophy extends App {

	val fileName = "day02/input.dat"
	implicit val input: List[String] = readInputLines(fileName) match {
		case Success(list) => list
		case Failure(_) => List()
	}

	val lineRegex = """(\d+)-(\d+) ([a-z]): (.+)""".r

	def countValidPasswords(check: => (Int, Int, Char, String) => Boolean)(implicit list: List[String]): Int = {
		var total = 0
		for (line <- list) {
			line match {
				case lineRegex(lower, upper, c, pass) => if (check(lower.toInt, upper.toInt, c(0), pass)) total += 1
				case _ => println(s"$line did not match $lineRegex")
			}
		}
		total
	}

	def countValidPasswordsRec(check: => (Int, Int, Char, String) => Boolean)(implicit list: List[String]): Int = {
		@tailrec
		def countValidPasswordsRec(list: List[String], check: => (Int, Int, Char, String) => Boolean, acc: Int): Int = {
			list match {
				case Nil => acc
				case x :: xs =>
					x match {
						case lineRegex(l, u, c, pw) =>
							countValidPasswordsRec(xs, check, acc + (if (check(l.toInt, u.toInt, c(0), pw)) 1 else 0))
						case _ => acc
					}
			}
		}
		countValidPasswordsRec(list, check, 0)
	}

	def check1(lower: Int, upper: Int, c: Char, pw: String): Boolean = {
		val count = pw.count(_ equals c)
		count >= lower && count <= upper
	}

	def check2(lower: Int, upper: Int, c: Char, pw: String): Boolean = {
		pw(lower - 1) == c ^ pw(upper - 1) == c
	}

	val x = countValidPasswordsRec(check1)
	println(s"1st check: $x valid passwords")

	val y = countValidPasswordsRec(check2)
	println(s"2nd check: $y valid passwords")

}
