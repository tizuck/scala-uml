package scalameta.operations

import scalameta.CollectorContext
import scalameta.operations.parameters.ParameterListsCollector
import uml.Operation

import scala.meta.Ctor

case class PrimaryConstructorCollector(primaryCstr:Option[Operation])

object PrimaryConstructorCollector {
  def apply(prim:Ctor.Primary)(implicit context: CollectorContext): PrimaryConstructorCollector = {
    if(prim.paramss.nonEmpty) {
      val operationParameterLists = ParameterListsCollector(prim.paramss)
      if (context.cstrOrigin.isEmpty) {
        throw new IllegalStateException("context Constructor Origin is empty unexpectedly")
      }
      val cstr = uml.Operation(
        None,
        None,
        context.cstrOrigin.get,
        operationParameterLists.parameterLists,
        None,
        Some("constr"))

      new PrimaryConstructorCollector(Some(cstr))
    } else {
      new PrimaryConstructorCollector(None)
    }
  }
}


