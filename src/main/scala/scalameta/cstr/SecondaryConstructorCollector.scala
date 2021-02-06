package scalameta.cstr


import scalameta.parameters.ParamssCollector
import scalameta.util.context.CollectorContext
import uml.Operation

import scala.meta.Ctor

case class SecondaryConstructorCollector(ctor:Operation)

object SecondaryConstructorCollector {
  def apply(sctor:Ctor.Secondary)(implicit context:CollectorContext): SecondaryConstructorCollector = {
    val defName = "this"
    val params = ParamssCollector(sctor.paramss).parameterLists

    SecondaryConstructorCollector(
      Operation(
        None,
        None,
        defName,
        params,
        None,
        Nil,
        None
      )
    )
  }
}
