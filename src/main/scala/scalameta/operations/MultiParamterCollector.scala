package scalameta.operations

import plantuml.Parameter
import scalameta.CollectorContext

import scala.meta.Term

case class MultiParamterCollector(parameter:List[List[Parameter]])

object MultiParamterCollector {
  def apply(paramss:List[List[Term.Param]])(implicit context: CollectorContext) : MultiParamterCollector = {
    new MultiParamterCollector(List(Nil))
  }
}
