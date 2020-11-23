package scalameta.typeparams

import scalameta.stateless.TypeNameCollector
import scalameta.util.context.CollectorContext

import scala.meta.Type

case class TypeParamContextBoundsCollector(contextBounds:List[String],fixme:Boolean = false)

object TypeParamContextBoundsCollector {
  def apply(cBounds:List[Type])(implicit context:CollectorContext): TypeParamContextBoundsCollector = {
    TypeParamContextBoundsCollector(cBounds.map(t => TypeNameCollector(t).typeRep))
  }

}
