package rest

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import uml.UMLUnit

import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Representing a received Scala File holding the content of the file
 * as a Base64 encoded file and the name of the file. This is the information
 * necessary to process a file to a UML class diagram.
 *
 * @param contentBase64 encoded file content.
 * @param name file name.
 */
final case class ScalaFileEntry(contentBase64:String, name:String)

/**
 * Collection of received encoded Scala files.
 *
 * @param entries list of encoded Scala files.
 */
final case class ScalaFilesCollection(entries:List[ScalaFileEntry])

final case class ASTCreated(rep:String)

object ASTCreator {
  sealed trait Command
  final case class CreateAST(scalaFiles:ScalaFilesCollection,replyTo: ActorRef[ASTCreated]) extends Command

  def apply(): Behavior[Command] = astCreation()

  def astCreation() : Behavior[Command] =
    Behaviors.receiveMessage[Command]({
      case CreateAST(scalaFiles, replyTo) =>
        //try to decode all the encoded files and parse them using Scala meta
        scalaFiles
        .entries
        .map{ f =>
          val decoded = Base64.getDecoder.decode(f.contentBase64)
          val stringRepDecoded = new String(decoded,StandardCharsets.UTF_8)
          println(stringRepDecoded)
          (stringRepDecoded,f.name)
        }
        replyTo ! ASTCreated("Works.")
        //@todo add uml processing
        Behaviors.same
    })


}
