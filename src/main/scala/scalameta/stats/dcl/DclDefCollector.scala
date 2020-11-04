package scalameta.relationships.dcl

import scalameta.operations.parameters.ParamssCollector
import scalameta.stateless.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Operation, Relationship, UMLElement}

import scala.meta.Decl

case class DclDefCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext) extends BaseCollector

object DclDefCollector {
  def apply(dclDef :Decl.Def)(implicit context:CollectorContext): DclDefCollector = {
    val operationName = dclDef.name.value
    //@todo get template parameter
    val parametersLists = ParamssCollector(dclDef.paramss).parameterLists

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
