package scalameta.stats.init


import scalameta.stateless.{TargetTypeCollector, TypeNameCollector}
import scalameta.util.{BaseCollector, StateChangingCollector}
import scalameta.util.context.CollectorContext
import scalameta.stats.StatCollector
import uml._
import uml.externalReferences.{CClass, ClassDefRef, ClassType}

import scala.meta.{Case, Defn, Init, Term}

case class InitCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object InitCollector {
  def apply(init:Init)(implicit context:CollectorContext): InitCollector = {
    val extendedType = TargetTypeCollector(init.tpe)

    val classType:ClassType = extendedType.oTemplate.map {
      case _: Defn.Object => uml.externalReferences.Object
      case c: Defn.Class if c.mods.contains(Case) => uml.externalReferences.CCaseClass
      case _: Defn.Class => uml.externalReferences.CClass
      case _: Defn.Enum => uml.externalReferences.Enum
      case _: Defn.Trait => uml.externalReferences.Trait
      case _ => CClass
    }.getOrElse(CClass)

    val relationshipIdentifier =
      extendedType
        .boundTemplates
        .map{
          tbind => s"${tbind._1} -> ${tbind._2}"}
        .mkString(",")

    //@todo problem with matching inits argss with entities argss are default values
    //  this would mean a complete depiction of default values behaviour

    val mappedInitArgs = Option.when(init.argss.nonEmpty && init.argss.head.nonEmpty) {
      init
        .argss
        .flatten
        .map(_.syntax)
        .mkString(",")
        .prepended('[')
        .appended(']')
    }


    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(
        None,
        None,
        ClassRef(extendedType.target,extendedType.namespace),
        context.localCon.thisPointer.get,
        if(relationshipIdentifier.nonEmpty) Some(s"<<bind $relationshipIdentifier >>") else None,
        Without),
      if(mappedInitArgs.nonEmpty)List(Stereotype("ctorBind",List(TaggedValue("vals",mappedInitArgs.get)))) else Nil
    )

    val classDefRef = ClassDefRef(
      classType,extendedType.target,
      extendedType.namespace,
      extendedType.boundTemplates.map(_._1),
      extendedType.oTemplate
    )
    new InitCollector(
      inheritance :: Nil,
      context.withExternalReference(classDefRef)
    )

  }
}
