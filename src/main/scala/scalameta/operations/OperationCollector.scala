package scalameta.operations

import plantuml.Operation
import scalameta.CollectorContext
import scalameta.common.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}

import scala.meta.{Decl, Defn, Stat}

case class OperationCollector(operations:List[Operation])

object OperationCollector {
  def apply(stats:List[Stat])(implicit context: CollectorContext) : OperationCollector = {
    val operationss = for (stat <- stats) yield {
      stat match {
        case sDef:Decl.Def => List(DeclDefOperationCollector(sDef).operation)
        case _ => Nil
      }
    }
    new OperationCollector(operationss.flatten)
  }
}
