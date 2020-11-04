package scalameta.operations.parameters

import scalameta.util.context.CollectorContext
import uml.Parameter

import scala.meta.Term

case class ParamsCollector(parameters:List[Parameter])

object ParamsCollector {
  def apply(parameters: List[Term.Param])(implicit context : CollectorContext): ParamsCollector = {
    val params = for (parameter <- parameters) yield {
      ParamCollector(parameter).param
    }

    new ParamsCollector(params)
  }
}
