package app.frontend.parser

import app.frontend.{Command, Exclude, Filter, Github, GithubType, Help, HelpType, InputPath, Name, Not, OutputPath, Textual, Verbose, VerboseType}
import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

case class InputState(commands:List[Command])

class Rules(positions:Positions) extends ListParsers(positions) {

  override def whitespace: Parser[Any] = regex("[\\s]*".r)

  lazy val command:InputState => Parser[Command] = ins => (commandPre ~> (
    help(ins)
    | verbose(ins)
    | name(ins)
    | github(ins)
    | directory(ins)
    | filesPath(ins)
    | textualPre ^^ {_ => Textual()}
    | exclude(ins)
    )) ^^ { c =>
    if(ins.commands.exists(cOther => cOther.getClass.equals(c.getClass))){
      throw new IllegalArgumentException(
        s"Same command: $c may not be used twice. Try removing one of the commands."
      )
    } else {
      c
    }
  }

  lazy val commandPre: Parser[Unit] = (
    "--" ^^ { _ => () }
      | "-" ^^ { _ => () }
    )

  lazy val helpCommand : Parser[HelpType] = commandPre ~> (
    verbosePre ^^ {_ => VerboseType}
    | githubPre ^^ {_ => GithubType}
    )

  lazy val directory : InputState => Parser[OutputPath] = _ =>
    directoryPre ~> path ^^ OutputPath


  lazy val help : InputState => Parser[Help] = ins => {
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

  lazy val exclude : InputState => Parser[Filter] = i => {
    excludePre ~> filter(i)
  }

  lazy val regex : InputState => Parser[Filter] = _ => {
    "r" ~> """([^\s]*)""".r ^^ {r => Exclude(r.replaceAll("""\*""","([^\\s]*)").r)}
  }

  lazy val not : InputState => Parser[Filter] = i =>  {
    "not" ~> filter(i) ^^ Not
  }

  lazy val filter : InputState => Parser[Filter] = i => {
    not(i) | regex(i)
  }

  lazy val github : InputState => Parser[Github] = _ =>
    githubPre ~> confPathGithub ^^ { p => Github(p)}

  lazy val verbose : InputState => Parser[Verbose] = _ => verbosePre ^^ { _ => Verbose()}

  lazy val filesPath : InputState => Parser[InputPath] = _ => filespathPre ~> path ^^ InputPath

  lazy val name : InputState => Parser[Name] = _ => namePre ~> identifier ^^ Name

  lazy val confPathGithub : Parser[String] = {
    """([a-zA-Z0-9](([^\s\\/])*)[\\/]?)+([a-zA-Z0-9](([^\s\\/])*)[\\/]?).conf""".r
  }

  lazy val path : Parser[String] = {
    """([A-Z][:][\\/])?(([~a-zA-Z0-9]|(\.\.)|-)+[\\/]?)+""".r
  }

  lazy val identifier : Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r


  lazy val helpPre : Parser[String] = "help"|"h"
  lazy val githubPre : Parser[String] = "github"
  lazy val verbosePre : Parser[String] = "verbose"|"v"
  lazy val directoryPre : Parser[String] = "directory"|"d"
  lazy val filespathPre : Parser[String] = "filespath"|"fp"
  lazy val namePre : Parser[String] = "name"|"n"
  lazy val textualPre: Parser[String] = "textual"|"t"
  lazy val excludePre: Parser[String] = "exclude"|"e"

}
