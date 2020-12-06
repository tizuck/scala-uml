package app.frontend

import java.io.IOException

import app.github.Config
import pureconfig.ConfigSource
import pureconfig._
import pureconfig.generic.auto._

sealed trait Command {
  type T
}

sealed case class Github(path:String) extends Command {
  override type T = Github
}

sealed trait HelpType
case object GithubType extends HelpType
case object VerboseType extends HelpType

sealed case class Help(optCommand:Option[HelpType]) extends Command {
  override type T = Help
}

case class Verbose() extends Command {
  override type T = Verbose
}