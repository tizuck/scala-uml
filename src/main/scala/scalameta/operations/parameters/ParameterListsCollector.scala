package scalameta.operations.parameters

import plantuml.Parameter
import scalameta.CollectorContext

import scala.meta.Term

case class ParameterListsCollector(parameterLists:List[List[Parameter]])

object ParameterListsCollector {
  def apply(paramss:List[List[Term.Param]])(implicit context:CollectorContext): ParameterListsCollector = {
    val pparamss = for (params <- paramss) yield {
      ParameterListCollector(params).parameters
    }

    new ParameterListsCollector(pparamss)
  }
}
