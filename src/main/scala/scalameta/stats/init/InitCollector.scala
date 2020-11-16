package scalameta.stats.init


import scalameta.stateless.TypeNameCollector
import scalameta.util.{BaseCollector, StateChangingCollector}
import scalameta.util.context.CollectorContext
import scalameta.stats.StatCollector
import uml._

import scala.meta.{Defn, Init, Term}

case class InitCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext) extends BaseCollector

object InitCollector {
  def apply(init:Init)(implicit context:CollectorContext): InitCollector = {
    val extendedType = TypeNameCollector(init.tpe)

    //@todo adopt this adhoc solution to all possible cases
    val extendedTemplate =
      context
        .globalCon
        .find(extendedType.typeRep,None,context.localCon.currentNamespace,context.localCon.currentImports)

    //@todo adopt this adhoc solution to all possible cases
    val extendedUMLTemplate: Option[StatCollector] = extendedTemplate match {
      case Some(tp) => tp._2 match {
        case Some(stat) => Some(StatCollector(stat)(
          context
            .withOptionalThisPointer(None)
            .withNamespace(tp._1)
          ))
        case None => None
      }
      case None => None
    }

    /*val templateIsDefined : Boolean =
      context
        .localCon
        .definedTemplates
        .exists((n:NamedElement) => n.identifier.equals(extendedType.typeRep))*/


    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(
        None,
        None,
        if(extendedTemplate.isDefined){
            ClassRef(extendedType.typeRep,extendedTemplate.get._1)
        }
        else{
          ClassRef(extendedType.typeRep)
        },
        context.localCon.thisPointer.get,None,Without),None)

    new InitCollector(
      inheritance :: extendedUMLTemplate.map(s => s.definedElements).getOrElse(Nil),
      context
    )

    //@todo build in the init inheritance
  }
}
