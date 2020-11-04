package scalameta.stats.dcl

import scalameta.stats.util.AssociationInformation
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Association, Class, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.Decl.Var

case class DclVarCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext
                                       ) extends BaseCollector

object DclVarCollector {
  def apply(declVar:Var)(implicit context:CollectorContext): DclVarCollector = {
    val assocInfo = AssociationInformation(declVar.pats,declVar.decltpe)

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

    new DclVarCollector(relationships,newContext)
  }
}
