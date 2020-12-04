package util

import scala.io.Source
import scala.util.{Try, Using}

object Utils {

	def readInputLines(file: String): Try[List[String]] = {
		Using(Source.fromResource(file)) { source =>
			source.getLines.toList
		}
	}

	def readInputChars(file: String): Try[List[Char]] = {
		Using(Source.fromResource(file)) { source =>
			source.toList
		}
	}

}
