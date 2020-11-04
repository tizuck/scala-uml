package scalameta.stats.dcl

import scalameta.stats.util.AssociationInformation
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Association, Class, ClassRef, ConcreteClass, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.Decl.Var

case class DclVarCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext
                                       ) extends BaseCollector

object DclVarCollector {
  def apply(dclVar:Var)(implicit context:CollectorContext): DclVarCollector = {
    val assocInfo = AssociationInformation(dclVar.pats,dclVar.decltpe)

    val templateIsDefined : Boolean =
      context
        .localCon
        .definedTemplates
        .exists((n:NamedElement) => n.identifier.equals(assocInfo.pDeclType))

    println(s"Define association for: ${context.localCon.thisPointer.get} to: ${dclVar.decltpe} in context: $context ")

    val relationships = assocInfo.pSources.map{ s =>
        Relationship(
          Association,
          FromTo,
          RelationshipInfo(
            None,
            Some(assocInfo.targetMultiplicity),
            context.localCon.thisPointer.get,
            if(templateIsDefined){
              ConcreteClass(context.localCon.definedTemplates.find(_.identifier.equals(assocInfo.pDeclType)).get)
            }else{ClassRef(assocInfo.pDeclType)},
            Some(s),
            FromTo),
          Some("var"))
    }

    new DclVarCollector(relationships,context)
  }
}
