package scalameta.operations.parameters

import scalameta.mods.ParameterModsCollector
import scalameta.stateless.TypeNameCollector
import scalameta.util.context.CollectorContext
import uml.Parameter

import scala.meta.{Term, Type}

case class ParamCollector(param:Parameter)

object ParamCollector {
  def apply(param: Term.Param)(implicit context : CollectorContext): ParamCollector = {
    val paramName = param.name.value

    val paramType = TypeNameCollector(param.decltpe.getOrElse(Type.Placeholder(Type.Bounds(None,None)))).typeRep

    val stereotypes = ParameterModsCollector(param.mods)

    new ParamCollector(Parameter(paramName,paramType,stereotypes.stereotype))
  }
}
