package day08

import util.Utils.readInputLines

import scala.util.{Failure, Success}

object HandheldHalting extends App {

	val testInput: List[String] = List("nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6")
	val input: List[String] = readInputLines("day08/input.dat") match {
		case Success(l) => l
		case Failure(_) => List()
	}
	val instruction = """^(nop|acc|jmp) ([+-])(\d+)$""".r

	def runJmps(): (Int, Int, Boolean) = {
		val jmps = input.zipWithIndex.filter(e => e._1.startsWith("jmp"))
		var done = false
		var res: (Int, Int, Boolean) = (-1, -1, false)
		for (jmp <- jmps if !done) {
			val line = jmp._1 match {
				case instruction(first, op, value) => (first, op, value)
				case _ => ("a", "a", "a")
			}
			val force = input.patch(jmp._2, Seq("nop " + line._2 + line._3), 1)
			res = runBootSequence(force)
			done = res._3
		}
		res
	}

	val nopsRes = runJmps()
	println(nopsRes)

	def runBootSequence(input: List[String]): (Int, Int, Boolean) = {
		var acc = 0
		var before = 0
		var visited: Set[Int] = Set()
		var infiniteLoop: Boolean = false
		var done = false
		var i = 0

		do {
			val line = input(i)
			//println(s"$i - $line")
			before = acc
			infiniteLoop = visited contains i
			visited += i
			line match {
				case instruction(code, op, value) =>
					code match {
						case "acc" =>
							op match {
								case "+" =>
									acc += value.toInt
									i += 1
								case "-" =>
									acc -= value.toInt
									i += 1
								case _ => println(s"Invalid operator $op")
							}
						case "jmp" =>
							op match {
								case "+" => i += value.toInt
								case "-" => i -= value.toInt
								case _ => println(s"Invalid operator $op")
							}
						case "nop" => i += 1
						case _ => println(s"unknown instruction $code")
					}
				case "." => println(s"done, acc = $acc"); done = true
				case _ => println(s"could not parse $line")
			}
		} while (!infiniteLoop && i < input.length && !done)
		(before, acc, !infiniteLoop)
	}

	val res = runBootSequence(input)
	val before = res._1
	val acc = res._2
	val success = res._3

	//println(s"acc was $before, and is $acc now")
	//println(s"success: $success")

}
