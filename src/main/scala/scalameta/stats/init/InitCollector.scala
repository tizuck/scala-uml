package scalameta.relationships.inheritance

import scalameta.common.TypeNameCollector
import scalameta.{CollectorContext, StateChangingCollector}
import uml._

import scala.meta.Init

case class InheritanceCollector(inheritance:Relationship,
                                override val resultingContext: CollectorContext) extends StateChangingCollector

object InheritanceCollector {
  def apply(init:Init)(implicit context:CollectorContext): InheritanceCollector = {
    val extendedType = TypeNameCollector(init.tpe)

    //Define Template for inner call in case it has not been defined before
    val newContext = if(context.definedTemplates.forall( (n:NamedElement) => !n.identifier.equals(extendedType.typeRep) )) {
      context.copy(definedTemplates =  Class(false,extendedType.typeRep,List.empty,List.empty,List.empty,None,None) :: context.definedTemplates)
    } else {context}

    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(None,None, newContext.definedTemplates.find((n:NamedElement) => n.identifier.equals(extendedType.typeRep)).get,context.thisPointer.get,None,Without),None)
    new InheritanceCollector(inheritance,newContext)
  }
}
