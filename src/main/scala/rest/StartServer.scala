package rest

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route

import scala.util.{Failure, Success}

object StartServer {

  private def startHttpServer(routes: Route)(implicit system : ActorSystem[_]): Unit = {
    import system.executionContext

    val futureBinding = Http().newServerAt("localhost",8080).bind(routes)

    futureBinding.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info("Server online at http://{}:{}/", address.getHostString, address.getPort)
      case Failure(ex) =>
        system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }

  def main(args: Array[String]): Unit = {
    val rootBehavior = Behaviors.setup[Nothing]{
      context =>
        val astCreatorActor =context.spawn(ASTCreator(),"ASTCreatorActor")
        context.watch(astCreatorActor)

        val routes = new ASTRoutes(astCreatorActor)(context.system)
        startHttpServer(routes.astRoutes)(context.system)

        Behaviors.empty
    }

    val system = ActorSystem[Nothing](rootBehavior,"ScalaUMLServer")
  }
}
