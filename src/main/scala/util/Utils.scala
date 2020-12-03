package util

import scala.io.Source
import scala.util.{Try, Using}

object Utils {

	def readInput(file: String): Try[List[String]] = {
		Using(Source.fromResource(file)) { source =>
			source.getLines.toList
		}
	}

}
