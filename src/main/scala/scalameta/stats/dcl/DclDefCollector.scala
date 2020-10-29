package scalameta.relationships.dcl
import scalameta.CollectorContext
import scalameta.common.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}
import scalameta.operations.DeclDefOperationCollector
import scalameta.operations.parameters.ParameterListsCollector
import scalameta.relationships.RelationshipBaseCollector
import uml.{Operation, Relationship, UMLElement}

import scala.meta.Decl

case class DclDefCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext) extends RelationshipBaseCollector

object DclDefCollector {
  def apply(dclDef :Decl.Def)(implicit context:CollectorContext): DclDefCollector = {
    val operationName = dclDef.name.value
    //@todo get template parameter
    val parametersLists = ParameterListsCollector(dclDef.paramss).parameterLists

    val returnType = TypeNameCollector(dclDef.decltpe).typeRep

    val modificators = ModificatorsCollector(dclDef.mods).modificators

    val accessModifiers = AccessModifierCollector(dclDef.mods).accessModifier

    val op = uml.Operation(
      Option.when(modificators.nonEmpty)(modificators),
      accessModifiers,
      operationName,
      parametersLists,
      Some(returnType),
      None)

    new DclDefCollector(List(op),context)
  }
}
