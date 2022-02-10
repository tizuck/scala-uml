package rest

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import org.slf4j.LoggerFactory
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toPackageRep}

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.meta.Source
import scala.meta.parsers.Parsed

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

final case class ASTCreated(rep:UMLUnit)

object ASTCreator {
  sealed trait Command
  final case class CreateAST(scalaFiles:ScalaFilesCollection,replyTo: ActorRef[ASTCreated]) extends Command

  val logger = LoggerFactory.getLogger("ASTCreator")
  def apply(): Behavior[Command] = astCreation()

  def astCreation() : Behavior[Command] =
    Behaviors.receiveMessage[Command]({
      case CreateAST(scalaFiles, replyTo) =>
        //try to decode all the encoded files and parse them using Scala meta
        val parsed = scalaFiles
        .entries
          //decode files
        .map({ f =>
          val decoded = Base64.getDecoder.decode(f.contentBase64)
          val stringRepDecoded = new String(decoded,StandardCharsets.UTF_8)
          (stringRepDecoded,f.name)
        })
          .map(t => ((parseScalaFile(t._1),t._2)))
          .filter(_._1.isDefined)
          .map(t => (t._1.get,t._2))
        try {
          val umlUnit = SourcesCollector(parsed, "test").umlUnit
          val pRep = toPackageRep(umlUnit).value.asInstanceOf[UMLUnit]
          val cRep = umlMethods.insertCompanionObjects(pRep).value
          val aRep = toAssocRep(cRep).value.asInstanceOf[UMLUnit]
          replyTo ! ASTCreated(aRep)
          Behaviors.same
        } catch {
          case e:Exception =>
            logger.error(s"Ast construction failed on request from: ${replyTo.path.address}")
            Behaviors.empty
        }
    })

  private def parseScalaFile(file:String):Option[Source] = {
    import scala.meta.dialects

    val logger = LoggerFactory.getLogger("server-routing")
    dialects.Scala3(file).parse[Source] match {
      case Parsed.Success(s) => Some(s)
      case error: Parsed.Error =>
        dialects.Scala213(file).parse[Source] match {
          case Parsed.Success(s) => Some(s)
          case error: Parsed.Error =>
            logger.warn("Scalameta: File not compatible with Scala 2 or Scala 3.")
            None
        }
    }
  }


}
