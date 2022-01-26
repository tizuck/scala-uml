package rest

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{RejectionHandler, Route}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import uml.UMLUnit
import ASTCreator._
import akka.http.scaladsl.model.StatusCodes.BadRequest
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

class ASTRoutes(astCreator: ActorRef[ASTCreator.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

  private implicit val timeout: Timeout =
    Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

  def getUMLAst(scalaFilesCollection: ScalaFilesCollection):Future[ASTCreated] =
    astCreator.ask(CreateAST(scalaFilesCollection,_))

  val logger: Logger = LoggerFactory.getLogger("server-routing")

  import JSONFormats._

  val corssettings: CorsSettings = CorsSettings(ConfigFactory.parseString(
    """akka-http-cors {
      |        allow-generic-http-requests = true
      |        allow-credentials = true
      |        allowed-origins = "*"
      |        allowed-headers = "*"
      |        allowed-methods = ["POST"]
      |        exposed-headers = []
      |        max-age = 30 minutes
      |      }
      |""".stripMargin
  ))

  val handler: RejectionHandler = RejectionHandler.newBuilder()
    .handle{
      case r@_ =>
        logger.warn(s"Post request on route astconstruction rejected with reason: ${r}")
        cors(corssettings){complete(
          HttpResponse(BadRequest,entity = """{"msg":"Some Rejection happened, look on server side"}""")
        )}
    }
    .result()

  val astRoutes : Route =
      pathPrefix("astconstruction"){
        handleRejections(handler){ cors(corssettings) {
          post {
            logger.info(s"Post Request arrived on route astconstruction")
            entity(as[ScalaFilesCollection]) { filesCollection =>
              onSuccess(getUMLAst(filesCollection)) { performed =>
                cors(corssettings){complete((StatusCodes.OK, performed))}
              }
            }
          }
        }}
      }
}
