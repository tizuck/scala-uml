package rest

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import uml.UMLUnit

import ASTCreator._

import scala.concurrent.Future

class ASTRoutes(astCreator: ActorRef[ASTCreator.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

  private implicit val timeout: Timeout =
    Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

  def getUMLAst(scalaFilesCollection: ScalaFilesCollection):Future[ASTCreated] =
    astCreator.ask(CreateAST(scalaFilesCollection,_))

  import JSONFormats._

  val astRoutes : Route =
    pathPrefix("astconstruction"){
      post{
        println("Entity arrived on POST: with filescollection: ")
        entity(as[ScalaFilesCollection]){filesCollection =>
          println(filesCollection)
          onSuccess(getUMLAst(filesCollection)){ performed =>
            complete((StatusCodes.OK,performed))
          }
        }
      }
    }
}
