package app.frontend.processor

import app.frontend.Help

import java.io.IOException
import pureconfig._
import pureconfig.generic.auto._
import uml.UMLUnit

sealed case class HelpProcessor(helpCmd: Help) extends Processor {

  import pureconfig._

  case class CommandAssignment(cmd: String, assignment: String)

  case class HelpAssignments(cmdAssignments: List[CommandAssignment])

  override def execute(): UMLUnit = {
    val conf = ConfigSource.file("src/main/resources/cmds.conf").load[HelpAssignments]
    val cmdAssignments = conf match {
      case Left(value) => throw new IOException(value.toString())
      case Right(value) => value
    }
    cmdAssignments.cmdAssignments.foldLeft(()) {
      case (_, ass) => println(s"${ass.cmd}\t\t\t\t${ass.assignment}")
    }
    UMLUnit("",Nil)
  }
}
