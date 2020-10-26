package scalameta.operations.parameters

import plantuml.Parameter
import scalameta.CollectorContext

import scala.meta.Term

case class ParameterListCollector(parameters:List[Parameter])

object ParameterListCollector {
  def apply(parameters: List[Term.Param])(implicit context : CollectorContext): ParameterListCollector = {
    val params = for (parameter <- parameters) yield {
      ParameterCollector(parameter).param
    }

    new ParameterListCollector(params)
  }
}
