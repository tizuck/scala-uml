package scalameta.operations.parameters

import scalameta.stateless.TypeNameCollector
import scalameta.stereotypes.ImplicitStereotypeCollector
import scalameta.util.CollectorContext
import uml.Parameter

import scala.meta.{Term, Type}

case class ParameterCollector(param:Parameter)

object ParameterCollector {
  def apply(param: Term.Param)(implicit context : CollectorContext): ParameterCollector = {
    val paramName = param.name.value

    val paramType = TypeNameCollector(param.decltpe.getOrElse(Type.Placeholder(Type.Bounds(None,None)))).typeRep

    val isImplicit = ImplicitStereotypeCollector(param.mods).isImplicitStereotype

    new ParameterCollector(Parameter(paramName,paramType,if (isImplicit) Some("implicit") else None))
  }
}
