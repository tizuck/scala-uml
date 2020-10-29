package scalameta.stats.dcl
import scalameta.CollectorContext
import scalameta.common.RelationshipBaseCollector
import scalameta.stats.util
import uml.{Association, Class, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.Decl.Var

case class DclVarRelationshipCollector(override val definedElements: List[UMLElement],
                                       override val resultingContext: CollectorContext
                                       ) extends RelationshipBaseCollector

object DclVarRelationshipCollector {
  def apply(declVar:Var)(implicit context:CollectorContext): DclVarRelationshipCollector = {
    val assocInfo = util.AssociationInformation(declVar.pats,declVar.decltpe)

    //Define Template for inner call in case it has not been defined before
    val newContext = if(context.definedTemplates.forall( (n:NamedElement) => !n.identifier.equals(assocInfo.pDeclType) )) {
      context.copy(definedTemplates =  Class(false,assocInfo.pDeclType,List.empty,List.empty,List.empty,None,None) :: context.definedTemplates)
    } else {context}

    println(s"Define association for: ${context.thisPointer.get.identifier} to: ${declVar.decltpe} in context: $newContext ")

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
          Some("var"))
    }

    new DclVarRelationshipCollector(relationships,newContext)
  }
}
