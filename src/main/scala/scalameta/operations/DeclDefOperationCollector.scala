package scalameta.operations

import plantuml.Operation
import scalameta.CollectorContext
import scalameta.common.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}

import scala.meta.Decl

case class DeclDefOperationCollector(operation:Operation)

object DeclDefOperationCollector {
  def apply(sDef:Decl.Def)(implicit context: CollectorContext) : DeclDefOperationCollector = {
    val operationName = sDef.name.value
    //@todo get template parameter
    val parametersLists = MultiParamterCollector(sDef.paramss).parameter

    val returnType = TypeNameCollector(sDef.decltpe).typeRep

    val modificators = ModificatorsCollector(sDef.mods).modificators

    val accessModifiers = AccessModifierCollector(sDef.mods).accessModifier

    val op = Operation(
      Option.when(modificators.nonEmpty)(modificators),
      accessModifiers,
      operationName,
      parametersLists,
      returnType
    )(None)
    //@todo if for example the context say we are in a constructor, then add the <<Constr>> stereotype
    new DeclDefOperationCollector(op)
  }
}
