package scalameta.stats.init


import scalameta.stateless.TypeNameCollector
import scalameta.util.StateChangingCollector
import scalameta.util.context.CollectorContext
import uml._

import scala.meta.{Defn, Init, Term}

case class InitCollector(inheritance:Relationship,
                         override val resultingContext: CollectorContext) extends StateChangingCollector

object InitCollector {
  def apply(init:Init)(implicit context:CollectorContext): InitCollector = {
    val extendedType = TypeNameCollector(init.tpe)

    val templateIsDefined : Boolean =
      context
        .localCon
        .definedTemplates
        .exists((n:NamedElement) => n.identifier.equals(extendedType.typeRep))


    val inheritance = Relationship(
      Extension,
      ToFrom,
      RelationshipInfo(
        None,
        None,
        if(templateIsDefined){
          ConcreteClass(context
            .localCon
            .definedTemplates
            .find(n => n.identifier.equals(extendedType.typeRep))
            .get )
        }
        else{
          ClassRef(extendedType.typeRep)
        },
        context.localCon.thisPointer.get,None,Without),None)
    new InitCollector(inheritance,context)

    //@todo build in the init inheritance
  }
}
