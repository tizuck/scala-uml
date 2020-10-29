package scalameta.stats.dcl

import scalameta.common.RelationshipBaseCollector
import scalameta.stats.util
import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Association, Class, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.Decl

case class DclValRelationshipCollector(override val definedElements : List[UMLElement],
                                       override val resultingContext: CollectorContext
                                       ) extends RelationshipBaseCollector

object DclValRelationshipCollector {
  def apply(dclVal:Decl.Val)(implicit context:CollectorContext): DclValRelationshipCollector = {
    val assocInfo = util.AssociationInformation(dclVal.pats,dclVal.decltpe)

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
    new DclValRelationshipCollector(relationships,newContext)
  }
}
