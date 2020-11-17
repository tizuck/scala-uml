package scalameta.stats.dcl

import scalameta.stats.StatCollector
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

    val statRep: Option[StatCollector] = assocInfo.pDeclType.oTemplate.map{
      StatCollector(_)(
        context
          .withOptionalThisPointer(None)
          .withNamespace(assocInfo.pDeclType.namespace)
      )
    }

    val relationshipIdentifier =
      assocInfo.pDeclType
        .boundTemplates
        .map{
          tbind => s"${tbind._1} -> ${tbind._2}"}
        .mkString(",")

    val relationships = assocInfo.pSources.map{ s =>
        Relationship(
          Association,
          FromTo,
          RelationshipInfo(
            None,
            Some(assocInfo.targetMultiplicity),
            context.localCon.thisPointer.get,
            ClassRef(assocInfo.pDeclType.target,assocInfo.pDeclType.namespace),
            Some(s"$s  ${if(relationshipIdentifier.nonEmpty)s"<<bind $relationshipIdentifier >>" else ""}"),
            FromTo),
          Some("var"))
    }

    new DclVarCollector(relationships,context)
  }
}
