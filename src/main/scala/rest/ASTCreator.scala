package rest

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import app.ci.Config
import uml.UMLUnit

import java.io.File

final case class ScalaFileEntry(file:String,name:String)
final case class ScalaFilesCollection(entries:List[ScalaFileEntry])

object ASTCreator {
  sealed trait Command
  final case class CreateAST(scalaFiles:ScalaFilesCollection,replyTo: ActorRef[UMLUnit]) extends Command

  def apply(): Behavior[Command]

  def astCreation() : Behavior[Command] =
    Behaviors.receiveMessage[Command]{
      case CreateAST(scalaFiles, replyTo) =>
        throw new NotImplementedError()
    }


}
