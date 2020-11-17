package scalameta.stats.dcl



import scalameta.stateless.TargetTypeCollector
import scalameta.stats.StatCollector
import scalameta.stats.util.AssociationInformation
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.externalReferences.{CClass, ClassDefRef, ClassType}
import uml.{Association, Class, ClassRef, ConcreteClass, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, UMLElement}

import scala.meta.{Case, Decl, Defn}

case class DclValCollector(override val definedElements : List[UMLElement],
                           override val resultingContext: CollectorContext
                                       ) extends BaseCollector

object DclValCollector {
  def apply(dclVal:Decl.Val)(implicit context:CollectorContext): DclValCollector = {
    val assocInfo = AssociationInformation(dclVal.pats,dclVal.decltpe)

    /*val statRep: Option[StatCollector] = assocInfo.pDeclType.oTemplate.map{
      StatCollector(_)(
        context
          .withOptionalThisPointer(None)
          .withNamespace(assocInfo.pDeclType.namespace)
      )
    }*/

    val relationshipIdentifier =
      assocInfo.pDeclType
        .boundTemplates
        .map{
          tbind => s"${tbind._1} -> ${tbind._2}"}
        .mkString(",")

    val name = assocInfo.pDeclType.target
    val namespace = assocInfo.pDeclType.namespace
    val oStat = assocInfo.pDeclType.oTemplate
    val classType:ClassType = oStat.map {
      case _: Defn.Object => uml.externalReferences.Object
      case c: Defn.Class if c.mods.contains(Case) => uml.externalReferences.CCaseClass
      case _: Defn.Class => uml.externalReferences.CClass
      case _: Defn.Enum => uml.externalReferences.Enum
      case _: Defn.Trait => uml.externalReferences.Trait
      case _ => CClass
    }.getOrElse(CClass)

    val templateParameter = assocInfo.pDeclType.boundTemplates.map(_._1)

    val relationships = assocInfo.pSources.map{ s =>
        Relationship(
          Association,
          FromTo,
          RelationshipInfo(
            None,
            Some(assocInfo.targetMultiplicity),
            context.localCon.thisPointer.get,
            ClassRef(name,namespace),
            Some(s"$s ${if(relationshipIdentifier.nonEmpty)s"<<bind $relationshipIdentifier >>" else ""}"),
            FromTo),
          None)
    }

    new DclValCollector(relationships ++ List(ClassDefRef(classType,name,namespace,templateParameter,oStat)),context)
  }
}
