package app.frontend

import scala.util.matching.Regex

sealed trait Command

sealed case class Github(path:String) extends Command

sealed trait HelpType
case object GithubType extends HelpType
case object VerboseType extends HelpType

sealed case class Help(optCommand:Option[HelpType]) extends Command

sealed case class Verbose() extends Command

sealed case class OutputPath(path:String) extends Command

sealed case class Name(name:String) extends Command

sealed case class InputPath(path:String) extends Command

sealed case class Textual() extends Command

sealed case class Exclude(regex:Regex) extends Command