package scalameta.relationships.dcl

import scalameta.operations.parameters.ParamssCollector
import scalameta.stateless.{AccessModifierCollector, ModificatorsCollector, TypeNameCollector}
import scalameta.typeparams.TypeParamsCollector
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

    val typeParamsCollector = TypeParamsCollector(dclDef.tparams)
    val typeParams = Option.when(typeParamsCollector.typeParams.nonEmpty)(typeParamsCollector.typeParams)


    val returnType = if(context.localCon.typeRequired){Some(TypeNameCollector(dclDef.decltpe).typeRep)}else None

    val modificators = ModificatorsCollector(dclDef.mods).modificators

    val accessModifiers = AccessModifierCollector(dclDef.mods).accessModifier

    val op = uml.Operation(
      Option.when(modificators.nonEmpty)(modificators),
      accessModifiers,
      operationName,
      parametersLists,
      returnType,
      Nil,
      typeParams)

    new DclDefCollector(List(op),context)
  }
}
