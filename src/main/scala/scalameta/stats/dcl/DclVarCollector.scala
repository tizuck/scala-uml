package scalameta.stats.dcl

import scalameta.stats.StatCollector
import scalameta.stats.util.AssociationInformation
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.externalReferences.{CClass, ClassDefRef, ClassType}
import uml.{Association, Class, ClassRef, ConcreteClass, FromTo, NamedElement, Operation, Relationship, RelationshipInfo, Stereotype, UMLElement}

import scala.meta.{Case, Defn}
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
            ClassRef(assocInfo.pDeclType.target,assocInfo.pDeclType.namespace),
            Some(s"$s  ${if(relationshipIdentifier.nonEmpty)s"<<bind $relationshipIdentifier >>" else ""}"),
            FromTo),
          List(Stereotype("var",Nil)))
    }

    new DclVarCollector(
      relationships,context.withExternalReference(ClassDefRef(classType,name,namespace,templateParameter,oStat))
    )
  }
}
