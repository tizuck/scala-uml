package scalameta.operations

import scalameta.operations.parameters.ParamssCollector
import scalameta.util.context.CollectorContext
import uml.Operation

import scala.meta.Ctor

case class PrimaryConstructorCollector(primaryCstr:Option[Operation])

object PrimaryConstructorCollector {
  def apply(prim:Ctor.Primary)(implicit context: CollectorContext): PrimaryConstructorCollector = {
    if(prim.paramss.nonEmpty) {
      val operationParameterLists = ParamssCollector(prim.paramss)
      if (context.localCon.cstrOrigin.isEmpty) {
        throw new IllegalStateException("context Constructor Origin is empty unexpectedly")
      }
      val cstr = uml.Operation(
        None,
        None,
        context.localCon.cstrOrigin.get,
        operationParameterLists.parameterLists,
        None,
        Some("constr"))

      new PrimaryConstructorCollector(Some(cstr))
    } else {
      new PrimaryConstructorCollector(None)
    }
  }
}


