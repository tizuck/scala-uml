package scalameta.operations.parameters

import scalameta.util.CollectorContext
import uml.Parameter

import scala.meta.Term

case class ParamssCollector(parameterLists:List[List[Parameter]])

object ParamssCollector {
  def apply(paramss:List[List[Term.Param]])(implicit context:CollectorContext): ParamssCollector = {
    val pparamss = for (params <- paramss) yield {
      ParamsCollector(params).parameters
    }

    new ParamssCollector(pparamss)
  }
}
