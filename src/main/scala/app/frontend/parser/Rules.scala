package app.frontend.parser

import app.frontend.{Command, Github, GithubType, Help, HelpType, Verbose, VerboseType}
import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

class Rules(positions:Positions) extends ListParsers(positions) {

  override def whitespace: Parser[Any] = regex("[\\s]*".r)

  lazy val commandPre : Parser[Unit] = (
    "--" ^^ {_ => ()}
    | "-" ^^ {_ => ()}
    )

  lazy val commands : Parser[List[Command]] = command.+

  lazy val command : Parser[Command] =
    commandPre ~> (
      help
      |github
      |verbose
    )

  lazy val helpCommand : Parser[HelpType] = (
    commandPre ~> (
      verbosePre ^^ {_ => VerboseType}
      | githubPre ^^ {_ => GithubType}
      )
  )

  lazy val help : Parser[Help] =
    helpPre ~> helpCommand.? ^^ Help

  lazy val github : Parser[Github] =
    githubPre ~> confPathGithub ^^ { p => Github(p)}

  lazy val verbose : Parser[Verbose] = verbosePre ^^ { _ => Verbose()}

  lazy val confPathGithub : Parser[String] = """((.*)/)*github.conf""".r

  lazy val helpPre : Parser[String] = "help"
  lazy val githubPre : Parser[String] = "github"
  lazy val verbosePre : Parser[String] = "verbose"

}
