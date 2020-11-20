package scalameta.typeparams

import scalameta.util.context.CollectorContext
import uml.GenericParameter

import scala.meta.Type

case class TypeParamsCollector(typeParams:List[GenericParameter],fixme:String="fixmefixmefixme")

object TypeParamsCollector {
  def apply(tparams: List[Type.Param])(implicit context:CollectorContext): TypeParamsCollector = {
    tparams.foldLeft(new TypeParamsCollector(Nil)){
      case (acc,tparam) =>
        val generic = TypeParamCollector(tparam).typeParam
        acc.copy(acc.typeParams ++ List(generic))
    }
  }
}