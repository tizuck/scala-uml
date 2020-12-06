package app.frontend.parser

import app.frontend.{Command, Github, GithubType, Help, HelpType, Verbose, VerboseType}
import cats.data.State
import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

case class InputState(commands:List[Command])

class Rules(positions:Positions) extends ListParsers(positions) {

  override def whitespace: Parser[Any] = regex("[\\s]*".r)

  lazy val command:InputState => Parser[Command] = ins => (
    (commandPre ~> (help(ins)
    | verbose(ins)
    | github(ins)
      )) ^^ { c =>
      if(ins.commands.exists(cOther => cOther.getClass.equals(c.getClass))){
        throw new IllegalArgumentException(
          s"[error] Same command: $c may not be used twice. Try removing one of the commands."
        )
      } else {
        c
      }
    }
  )

  lazy val commandPre: Parser[Unit] = (
    "--" ^^ { _ => () }
      | "-" ^^ { _ => () }
    )

  lazy val helpCommand : Parser[HelpType] = (
    commandPre ~> (
      verbosePre ^^ {_ => VerboseType}
      | githubPre ^^ {_ => GithubType}
      )
  )

  lazy val help : InputState => Parser[Help] = ins => {
    println("HereinHelp")
      helpPre ~> helpCommand.? ^^ {
        optHC =>
          if(!ins.commands.exists{case _:Help => true case _ => false}) {
            Help(optHC)
          } else {
            throw new IllegalArgumentException("--help cannot be used together with other commands." +
              " try: --help as sole command for help or\n" +
              " --help <command> for help to specific command.")
          }
    }
  }

  lazy val github : InputState => Parser[Github] = ins =>
    githubPre ~> confPathGithub ^^ { p => Github(p)}

  lazy val verbose : InputState => Parser[Verbose] = ins => verbosePre ^^ { _ => Verbose()}

  lazy val confPathGithub : Parser[String] =
  """([a-zA-Z0-9](([^\s\\/])*)[\\/]?)+([a-zA-Z0-9](([^\s\\/])*)[\\/]?).conf""".r

  lazy val helpPre : Parser[String] = "help"
  lazy val githubPre : Parser[String] = "github"
  lazy val verbosePre : Parser[String] = "verbose"

}
