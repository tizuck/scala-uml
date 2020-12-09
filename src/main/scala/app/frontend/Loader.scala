package app.frontend

import app.frontend.exceptions.InvalidParameterException

import java.io.Reader
import app.frontend.parser.{InputState, Rules}
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.{Positions, Source, StringSource}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

case class Loader(commands:List[Command])

object Loader {

  def apply(args:Array[String]):Loader = {
    if(args.isEmpty){
      new Loader(List.empty[Command])
    } else {
      val fullParams = args.mkString(" ")
      val res = parseInputSeq(StringSource(fullParams), InputState(Nil), new Rules(new Positions))
      Loader(res)
    }
  }


  /**
   * Parses commands provided as console arguments.
   *
   * Each recursive step consumes exactly one command
   * and validates its correctness respecting the
   * previously parsed commands.
   *
   * Note that the sequence of command may not contain newline character.
   *
   * @param inputSource Source containing a chain of commands.
   * @param inputState Current state containing all parsed commands.
   * @param parserRules Rules defining how to parse commands.
   * @return A valid sequence of commands.
   * @throws java.lang.IllegalArgumentException if sequence of commands is illegal. Provides detailed information
   *                                            in message of exception.
   */
  private def parseInputSeq(inputSource:Source, inputState:InputState, parserRules:Rules) : List[Command] = {
      val parse = parserRules.parse(parserRules.command(inputState),inputSource)
      val (parseRes,next) = parse match {
        case Success(result, next) => (result,next)
        case success: NoSuccess =>
          val logger = LoggerFactory.getLogger("execution")
          throw new InvalidParameterException(s"input: ${inputSource.content} could not be processed." +
            s" Try --help to get a list of available commands.",success)
      }
      val newInputState = inputState.copy(commands = inputState.commands.appended(parseRes))
      if(next.found.equals("end of source")){
        parseRes :: Nil
      } else {
        val nextString = next.nextPosition.source.content.substring(next.nextPosition.column - 1)
        parseRes :: parseInputSeq(StringSource(nextString),newInputState,parserRules)
      }
  }
}
