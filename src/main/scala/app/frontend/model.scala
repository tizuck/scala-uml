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

trait Filter extends Command {
  def matches(s:String):Boolean
}

sealed case class Exclude(regex:Regex) extends Filter {
  override def matches(s: String): Boolean = regex.matches(s)
}

sealed case class Not(f:Filter) extends Filter {
  override def matches(s: String): Boolean = !f.matches(s)
}