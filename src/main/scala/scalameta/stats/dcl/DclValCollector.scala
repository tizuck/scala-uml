package scalameta.stats.dcl



import scalameta.stats.util.AssociationInformation
import scalameta.util.{BaseCollector, CollectorContext}
import uml.{Association, Class, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.Decl

case class DclValCollector(override val definedElements : List[UMLElement],
                           override val resultingContext: CollectorContext
                                       ) extends BaseCollector

object DclValCollector {
  def apply(dclVal:Decl.Val)(implicit context:CollectorContext): DclValCollector = {
    val assocInfo = AssociationInformation(dclVal.pats,dclVal.decltpe)

    val newContext = if(context.definedTemplates.forall( (n:NamedElement) => !n.identifier.equals(assocInfo.pDeclType) )) {
      context.copy(definedTemplates =  Class(false,assocInfo.pDeclType,List.empty,List.empty,List.empty,None,None) :: context.definedTemplates)
    } else {context}

    println(s"Define association for: ${context.thisPointer.get.identifier} to: ${dclVal.decltpe} in context: $newContext ")
    val relationships = assocInfo.pSources.map{ s =>
        Relationship(
          Association,
          FromTo,
          RelationshipInfo(
            None,
            Some(assocInfo.targetMultiplicity),
            newContext.thisPointer.get,
            newContext.definedTemplates.find((n:NamedElement) => n.identifier.equals(assocInfo.pDeclType)).get,
            Some(s),
            FromTo),
          None)
    }
    new DclValCollector(relationships,newContext)
  }
}
