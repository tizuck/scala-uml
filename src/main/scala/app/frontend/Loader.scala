package app.frontend

import java.io.Reader

import app.frontend.parser.Rules
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.{Positions, Source, StringSource}

case class Loader(commands:List[Command])

object Loader {
  def apply(args:Array[String]):Loader = {
    val fullParams = args.mkString(" ")
    val parserRules = new Rules(new Positions)
    val parsedInput = parserRules.parseAll(parserRules.commands,StringSource(fullParams))
    val commands = parsedInput match {
      case Success(result, _) => result
      case error: NoSuccess => throw new IllegalArgumentException("input could not be processed with failure: " + error)
    }
    Loader(commands)
  }
}
